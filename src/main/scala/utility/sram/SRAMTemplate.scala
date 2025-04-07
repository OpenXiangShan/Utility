/**************************************************************************************
* Copyright (c) 2020 Institute of Computing Technology, CAS
* Copyright (c) 2020 University of Chinese Academy of Sciences
*
* NutShell is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*             http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
* FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package utility.sram

import scala.util.Random
import chisel3._
import chisel3.util._
import sourcecode.FullName
import utility.mbist.MbistClockGateCell
import utility.{ClockGate, GatedValidRegNext, HoldUnless, LFSR64}

class SRAMBundleA(val set: Int) extends Bundle {
  val setIdx = Output(UInt(log2Up(set).W))

  def apply(setIdx: UInt) = {
    this.setIdx := setIdx
    this
  }
}

class SRAMBundleAW[T <: Data](private val gen: T,
  set: Int,
  val way: Int = 1,
  val useBitmask: Boolean = false
) extends SRAMBundleA(set) {

  private val dataWidth = gen.getWidth

  val data: Vec[T] = Output(Vec(way, gen))
  val waymask: Option[UInt] = if (way > 1) Some(Output(UInt(way.W))) else None
  // flattened_bitmask is the flattened form of [waymask, bitmask], can be use directly to mask memory
  val flattened_bitmask: Option[UInt] = if (useBitmask) Some(Output(UInt((way * dataWidth).W))) else None
  // bitmask is the original bitmask passed from parameter
  val bitmask: Option[UInt] = if (useBitmask) Some(Output(UInt((dataWidth).W))) else None

  def apply(data: Vec[T], setIdx: UInt, waymask: UInt): SRAMBundleAW[T] = {
    require(waymask.getWidth == way,
     s"waymask width does not equal nWays, waymask width: ${waymask.getWidth}, nWays: ${way}")
    super.apply(setIdx)
    this.data := data
    this.waymask.foreach(_ := waymask)
    this
  }

  def apply(data: Vec[T], setIdx: UInt, waymask: UInt, bitmask: UInt): SRAMBundleAW[T] = {
    require(useBitmask, "passing bitmask when not using bitmask")
    require(bitmask.getWidth == dataWidth,
      s"bitmask width does not equal data width, bitmask width: ${bitmask.getWidth}, data width: ${dataWidth}")
    apply(data, setIdx, waymask)
    this.flattened_bitmask.foreach(_ :=
      VecInit.tabulate(way * dataWidth)(n => waymask(n / dataWidth) && bitmask(n % dataWidth)).asUInt)
    this.bitmask.foreach(_ := bitmask)
    this
  }

  // this could only be used when waymask is onehot or nway is 1
  def apply(data: T, setIdx: UInt, waymask: UInt): SRAMBundleAW[T] = {
    apply(VecInit(Seq.fill(way)(data)), setIdx, waymask)
    this
  }

  def apply(data: T, setIdx: UInt, waymask: UInt, bitmask: UInt): SRAMBundleAW[T] = {
    apply(VecInit(Seq.fill(way)(data)), setIdx, waymask, bitmask)
    this
  }
}

class SRAMBundleR[T <: Data](private val gen: T, val way: Int = 1) extends Bundle {
  val data = Output(Vec(way, gen))
}

class SRAMReadBus[T <: Data](private val gen: T, val set: Int, val way: Int = 1) extends Bundle {
  val req = Decoupled(new SRAMBundleA(set))
  val resp = Flipped(new SRAMBundleR(gen, way))

  def apply(valid: Bool, setIdx: UInt) = {
    this.req.bits.apply(setIdx)
    this.req.valid := valid
    this
  }
}

class SRAMWriteBus[T <: Data](
  private val gen: T, val set: Int,
  val way: Int = 1, val useBitmask: Boolean = false
) extends Bundle {
  val req = Decoupled(new SRAMBundleAW(gen, set, way, useBitmask))

  def apply(valid: Bool, data: Vec[T], setIdx: UInt, waymask: UInt): SRAMWriteBus[T] = {
    this.req.bits.apply(data = data, setIdx = setIdx, waymask = waymask)
    this.req.valid := valid
    this
  }

  def apply(valid: Bool, data: Vec[T], setIdx: UInt, waymask: UInt, bitmask: UInt): SRAMWriteBus[T] = {
    this.req.bits.apply(data = data, setIdx = setIdx, waymask = waymask, bitmask = bitmask)
    this.req.valid := valid
    this
  }

  def apply(valid: Bool, data: T, setIdx: UInt, waymask: UInt): SRAMWriteBus[T] = {
    apply(valid, VecInit(Seq.fill(way)(data)), setIdx, waymask)
    this
  }

  def apply(valid: Bool, data: T, setIdx: UInt, waymask: UInt, bitmask: UInt): SRAMWriteBus[T] = {
    apply(valid, VecInit(Seq.fill(way)(data)), setIdx, waymask, bitmask)
    this
  }
}

/** Describes the behavior of a dual-port SRAM when there is a simultaneous read and write to the same index. */
object SRAMConflictBehavior extends Enumeration {
  // based on example given in Enumeration documentation
  protected case class BehaviorVal(macroAllowsConflict: Boolean) extends super.Val
  import scala.language.implicitConversions
  implicit def valueToBehaviorVal(x: Value): BehaviorVal = x.asInstanceOf[BehaviorVal]

  type SRAMConflictBehavior = Value

  /** Allow read-write conflicts, but all read data is corrupt. */
  val CorruptRead = BehaviorVal(true)

  /** Allow read-write conflicts, but read data is corrupt for ways that were written to. */
  val CorruptReadWay = BehaviorVal(true)

  /** Allow read-write conflicts, and bypass write data to read data. This assumes the underlying macro supports
    * read-write conflicts and behaves like CorruptReadWay, since otherwise we cannot recover read data for ways that
    * were not written to.
    */
  val BypassWrite = BehaviorVal(true)

  /** Allow read-write conflicts, but the underlying macro does not support it. On a conflict, save write data into a
    * single-entry buffer. In the following cycle, stall external reads and writes, and write the buffer contents to the
    * RAM.
    *
    * This option requires connected logic to support read and write stalls.
    */
  val BufferWrite = BehaviorVal(false)

  /** Allow read-write conflicts, but the underlying macro does not support it. On a conflict, save the write data into a
    * single-entry buffer. The buffer contents stay valid until they can be written to the RAM without conflicting with
    * the read. If a second conflict occurs while the buffer is valid, the buffer contents are replaced, and the first
    * write is lost!
    *
    * This approach has worse timing than BufferWrite, but does not require connected logic to support read and write stalls.
    */
  val BufferWriteLossy = BehaviorVal(false)

  /** The same as BufferWriteLossy, but tries to improve timing by allowing more frequent loss of buffer contents. */
  val BufferWriteLossyFast = BehaviorVal(false)

  /** Do not allow read-write conflicts. Stall the write by de-asserting the ready signal on the write port. */
  val StallWrite = BehaviorVal(false)

  /** Do not allow read-write conflicts. Stall the read by de-asserting the ready signal on the read port. */
  val StallRead = BehaviorVal(false)

  /** Default value when parameter is not specified. */
  val DefaultBehavior = CorruptReadWay
}
import SRAMConflictBehavior._

/** Class for SRAM implementing
 * @param gen set the entry type
 * @param set set the array depth
 * @param way set the array way
 * @param singlePort set the SRAM to be single port. A Single port SRAM can not be written and read at the same time.
 * @param shouldReset set the SRAM to be resetable. A resetable SRAM will be clear to all zero after reset is deasserted.
 * @param extraReset add an extra reset port for the SRAM. Not applicable when [[shouldReset]] is false
 * @param holdRead store read value in an hold reg. Output data will use the value of the reg when no read is being served.
 * @param conflictBehavior behavior when read and write with the same address are concurrent. Not applicable in single port SRAM.
 * @param bypassWrite (for backward compatibility, should replace with conflictBehavior = BypassWrite) bypass the write to read when read and write with the same address are concurrent. Not applicable in single port SRAM.
 * @param useBitmask make one mask bit for every data bit.
 * @param useBitmask make one mask bit for every data bit.
 * @param withClockGate add clock gate to read and write ports, respectively.
 * @param separateGateClock no effect, only supports independent RW cg, only for API compatibility
 * @param hasMbist enable MBIST support.
 * @param latency output setup multicycle, witch means how many cycles the data is ready for capture after read sampling clock edge.
 * @param extraHold enable a extra input hold cycle. User should keep all input for one cycle if this option is enabled.
 * @param extClockGate expose clock control signals to IOs to support using external clock gate when doing MBIST.
 * @param hasSramCtl enable sram ctl support.
 * @param suffix add suffix in the name of SRAM wrapper
 */

class SRAMTemplate[T <: Data](
  gen: T, set: Int, way: Int = 1, singlePort: Boolean = false,
  shouldReset: Boolean = false, extraReset: Boolean = false,
  holdRead: Boolean = false, bypassWrite: Boolean = false,
  conflictBehavior: SRAMConflictBehavior = DefaultBehavior,
  useBitmask: Boolean = false, withClockGate: Boolean = false,
  separateGateClock: Boolean = false,
  hasMbist: Boolean = false, latency: Int = 1, extraHold:Boolean = false,
  extClockGate: Boolean = false, hasSramCtl: Boolean = false,
  suffix: Option[String] = None
)(implicit valName: sourcecode.FullName) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way, useBitmask))
    val broadcast = if(hasMbist) Some(new SramBroadcastBundle) else None
    val mbistCgCtl = if(hasMbist && extClockGate) {
      Some(new Bundle {
        val en = Output(Bool())
        val rckEn = Output(Bool())
        val wckEn = Output(Bool())
        val rclk = Input(Clock())
        val wclk = Input(Clock())
      })
    } else {
      None
    }
  })

  require(latency >= 1)

  // conflictBehavior is only meaningful for dual-port RAMs
  require(!(singlePort && conflictBehavior != DefaultBehavior))
  // bypassWrite is for backward compatibility, cannot use at the same time as conflictBehavior
  require(!(bypassWrite && conflictBehavior != DefaultBehavior))
  val finalConflictBehavior = if (bypassWrite) BypassWrite else conflictBehavior

  val extra_reset = if (extraReset) Some(IO(Input(Bool()))) else None

  val wordType = UInt(gen.getWidth.W)
  val arrayWidth = if (useBitmask) 1 else gen.getWidth
  val arrayType = UInt(arrayWidth.W)
  val arrayPortSize = if (useBitmask) way * gen.getWidth else way
  val sp = SramInfo(arrayWidth, arrayPortSize, hasMbist)

  private val implCg = !extClockGate && (extraHold || withClockGate)
  private val rcg = if(implCg) Some(Module(new MbistClockGateCell(extraHold))) else None
  private val wcg = if(!singlePort && implCg) Some(Module(new MbistClockGateCell(extraHold))) else None
  private val nodeNum = sp.mbistNodeNum
  private val rclk = io.mbistCgCtl.map(_.rclk).getOrElse(rcg.map(_.out_clock).getOrElse(clock))
  private val wclk = io.mbistCgCtl.map(_.wclk).getOrElse(wcg.map(_.out_clock).getOrElse(clock))
  private val (mbistBd, array, vname) = SramHelper.genRam(
    sp = sp,
    set = set,
    dp = !singlePort,
    setup = 1,
    hold = if(extraHold) 1 else 0,
    latency = latency,
    bist = hasMbist,
    broadcast = io.broadcast,
    hasSramCtl = hasSramCtl,
    rclk,
    Some(wclk),
    suffix = suffix.getOrElse(SramHelper.getSramSuffix(valName.value)),
    foundry = "Unknown",
    sramInst = "STANDARD",
    template = this
  )
  val sramName: String = vname
  private val brcBd = io.broadcast.getOrElse(0.U.asTypeOf(new SramBroadcastBundle))

  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }
    if (extra_reset.isDefined) {
      when (extra_reset.get) {
        _resetState := true.B
      }
    }

    resetState := _resetState
    resetSet := _resetSet
  }

  // conflict handling for dual-port SRAMs
  private val conflictEarlyS0 = WireInit(io.r.req.valid && io.w.req.valid)
  private val conflictRaddrS0 = WireInit(io.r.req.bits.setIdx)
  private val conflictWaddrS0 = WireInit(io.w.req.bits.setIdx)
  private val conflictWmaskS0 = WireInit(io.w.req.bits.waymask.getOrElse(1.U(1.W)))
  private val conflictWdataS0 = WireInit(io.w.req.bits.data.asTypeOf(Vec(way, wordType)))
  private val conflictValidS0 = WireInit(conflictEarlyS0 && conflictWmaskS0.orR && conflictRaddrS0 === conflictWaddrS0)

  private val conflictEnableS0 = WireInit(conflictEarlyS0)
  private val conflictEarlyS1 = RegNext(conflictEarlyS0, false.B)
  private val conflictRaddrS1 = RegEnable(conflictRaddrS0, io.r.req.valid) // must be updated on every read to support bypassing buffer contents
  private val conflictWaddrS1 = RegEnable(conflictWaddrS0, conflictEnableS0)
  private val conflictWmaskS1 = RegEnable(conflictWmaskS0, conflictEnableS0)
  private val conflictWdataS1 = RegEnable(conflictWdataS0, conflictEnableS0)
  private val conflictValidS1 = WireInit(conflictEarlyS1 && conflictWmaskS1.orR && conflictRaddrS1 === conflictWaddrS1)

  private val conflictInhibitWrite = WireInit(false.B)

  private val conflictBufferValid = WireInit(false.B)
  private val conflictBufferCanWrite = WireInit(false.B)
  private val conflictBufferWrite = WireInit(conflictBufferValid && conflictBufferCanWrite)

  private val ren = io.r.req.fire
  private val wen = io.w.req.valid && !conflictInhibitWrite || conflictBufferWrite || resetState

  private val _wmask = if (useBitmask) {
    io.w.req.bits.flattened_bitmask.getOrElse("b1".U)
  } else {
    io.w.req.bits.waymask.getOrElse("b1".U)
  }

  private val waddr = MuxCase(io.w.req.bits.setIdx, Seq(
    conflictBufferWrite -> conflictWaddrS1,
    resetState -> resetSet,
  ))
  private val wdata = MuxCase(io.w.req.bits.data.asUInt, Seq(
    conflictBufferWrite -> conflictWdataS1.asUInt,
    resetState -> 0.U,
  ))
  private val wmask = MuxCase(_wmask, Seq(
    conflictBufferWrite -> conflictWmaskS1,
    resetState -> Fill(arrayPortSize, true.B),
  ))

  private val raddr = io.r.req.bits.setIdx

  private val mbistWmask = sp.mbistMaskConverse(mbistBd.wmask, mbistBd.selectedOH)
  private val funcWmask = sp.funcMaskConverse(wmask)

  private val mbistWdata = Fill(nodeNum, mbistBd.wdata)
  private val ramWmask = if(hasMbist) Mux(mbistBd.ack, mbistWmask, funcWmask) else funcWmask
  private val ramWdata = if(hasMbist) Mux(mbistBd.ack, mbistWdata, wdata) else wdata
  private val ramWen = if(hasMbist) Mux(mbistBd.ack, mbistBd.we, wen) else wen
  private val ramRen = if(hasMbist) Mux(mbistBd.ack, mbistBd.re, ren) else ren
  private val ramRaddr = if(hasMbist) Mux(mbistBd.ack, mbistBd.addr_rd, raddr) else raddr
  private val ramWaddr = if(hasMbist & singlePort) {
    Mux(mbistBd.ack, mbistBd.addr_rd, waddr)
  } else if(hasMbist & !singlePort) {
    Mux(mbistBd.ack, mbistBd.addr, waddr)
  } else {
    waddr
  }

  private val rckEn = Wire(Bool())
  private val wckEn = Wire(Bool())
  io.mbistCgCtl.foreach(c => {
    c.en := mbistBd.ack
    c.rckEn := rckEn
    c.wckEn := wckEn
  })
  if(extraHold) {
    val rckEnReg = RegNext(rckEn, false.B)
    val wckEnReg = RegNext(wckEn, false.B)
    rckEn := Cat(ramRen, rckEnReg) === "b10".U
    wckEn := Cat(ramWen, wckEnReg) === "b10".U
  } else {
    rckEn := ramRen
    wckEn := ramWen
  }

  private val ramRdata = SramProto.read(array, singlePort, ramRaddr, ramRen)
  private val finalRamWen = ramWen && !brcBd.mbist.ram_hold
  when(finalRamWen) {
    SramProto.write(array, singlePort, ramWaddr, ramWdata, ramWmask)
  }

  private val respReg = RegInit(0.U(latency.W))
  when(rckEn) {
    respReg := (1 << (latency - 1)).U
  }.otherwise {
    respReg := Cat(false.B, respReg) >> 1.U
  }

  rcg.foreach(cg => {
    cg.dft.fromBroadcast(brcBd)
    cg.mbist.req := mbistBd.ack
    cg.mbist.readen := rckEn
    if(singlePort) {
      cg.mbist.writeen := wckEn
      cg.E := rckEn | wckEn
    } else {
      cg.mbist.writeen := false.B
      cg.E := rckEn
    }
  })

  wcg.foreach(cg => {
    cg.dft.fromBroadcast(brcBd)
    cg.mbist.req := mbistBd.ack
    cg.mbist.readen := false.B
    cg.mbist.writeen := wckEn
    cg.E := wckEn
  })

  if (!singlePort && !finalConflictBehavior.macroAllowsConflict) {
    // normally, the conflict handling logic is not triggered if the write mask is all zeroes
    // but to be conservative, still treat that case as a conflict here
    assert(
      !(rckEn && wckEn && ramRen && finalRamWen && ramRaddr === ramWaddr),
      "A read-write conflict occurred on an SRAM that does not allow it (index=0x%x)",
      ramRaddr)
  }

  private val raw_rdata = ramRdata.asTypeOf(Vec(way, wordType))
  require(wdata.getWidth == ramRdata.getWidth)

  val randomData = VecInit(Seq.tabulate(way) { i =>
    var seed = new Random(i).nextLong()
    if (seed < 0) seed = -seed
    LFSR64(increment=io.r.req.valid, seed=Some(seed)).asTypeOf(wordType)
  })

  // conflict handling for dual-port SRAMs
  val conflictStallRead = WireInit(false.B)
  val conflictStallWrite = WireInit(false.B)
  val bypassEnable = WireInit(conflictValidS1)
  val bypassMask = WireInit(conflictWmaskS1)
  val bypassData = WireInit(conflictWdataS1)

  finalConflictBehavior match {
    case CorruptRead => {
      bypassMask := Fill(way, 1.U(1.W))
      bypassData := randomData
    }
    case CorruptReadWay => {
      bypassData := randomData
    }
    case BypassWrite => // Handled elsewhere
    case BufferWrite => {
      conflictInhibitWrite := conflictValidS0
      // Stall reads and writes when the buffer is valid, which guarantees it can be written to the RAM immediately
      conflictBufferValid := conflictValidS1
      conflictBufferCanWrite := true.B
      conflictStallRead := conflictBufferValid
      conflictStallWrite := conflictBufferValid
    }
    case BufferWriteLossy => {
      conflictInhibitWrite := conflictValidS0
      conflictBufferValid := RegNext(conflictValidS0 || conflictBufferValid && (!conflictBufferCanWrite || io.w.req.valid), false.B)
      conflictBufferCanWrite := !(io.r.req.valid && conflictRaddrS0 === conflictWaddrS1)
      // Redirect any incoming write to the buffer during buffer writeback
      conflictEnableS0 := conflictValidS0 || conflictBufferValid && conflictBufferCanWrite
      bypassEnable := conflictBufferValid && RegNext(io.r.req.valid) && conflictRaddrS1 === conflictWaddrS1
    }
    case BufferWriteLossyFast => {
      conflictInhibitWrite := conflictEarlyS0
      conflictBufferValid := conflictValidS1 || RegNext(conflictBufferValid && (!conflictBufferCanWrite || conflictEnableS0), false.B)
      conflictBufferCanWrite := !(io.r.req.valid && conflictRaddrS0 === conflictWaddrS1)
      // Redirect any incoming write to the buffer when it is valid, to preserve ordering
      conflictEnableS0 := io.w.req.valid && (io.r.req.valid || conflictBufferValid)
      bypassEnable := conflictBufferValid && RegNext(io.r.req.valid) && conflictRaddrS1 === conflictWaddrS1
    }
    // These is bad for timing, but using a less precise signal here would basically give us a single-port RAM
    case StallWrite => {
      conflictInhibitWrite := conflictValidS0
      conflictStallWrite := conflictValidS0
      bypassEnable := false.B
    }
    case StallRead => {
      conflictInhibitWrite := conflictValidS0
      conflictStallRead := conflictValidS0
      bypassEnable := false.B
    }
  }

  val finalBypassMask = Fill(way, bypassEnable) & bypassMask.asTypeOf(UInt(way.W))
  val mem_rdata = if (singlePort) raw_rdata else {
    VecInit(finalBypassMask.asBools.zip(raw_rdata).zip(bypassData).map {
      case ((m, r), w) => Mux(m, w, r)
    })
  }

  // hold read data for SRAMs
  val rdata = (if (holdRead) HoldUnless(mem_rdata, GatedValidRegNext(ren))
              else mem_rdata).map(_.asTypeOf(gen))

  private val rdataReg = RegEnable(ramRdata, respReg(0))
  private val selectOHReg = RegEnable(mbistBd.selectedOH, respReg(0))
  mbistBd.rdata := Mux1H(selectOHReg, rdataReg.asTypeOf(Vec(nodeNum, UInt((ramRdata.getWidth / nodeNum).W))))
  io.r.resp.data := VecInit(rdata)

  private val singleHold = if(singlePort) io.w.req.valid else false.B
  private val resetHold = if(shouldReset) resetState else false.B
  io.r.req.ready := !resetHold && !singleHold && !conflictStallRead
  io.w.req.ready := !resetHold && !conflictStallWrite
}

/**
 * Split large SRAM into smaller ones. This class draws inspiration from coupledL2-SplittedSRAM
 * Since the SplittedSRAM class cannot be fully compatible with the parameters of the
 * FoldedSRAMTemplate class, the SplittedSRAMTemplate class is therefore defined.
 */
class SplittedSRAMTemplate[T <: Data]
(
  gen: T, set: Int, way: Int = 1,
  setSplit: Int = 1, waySplit: Int = 1, dataSplit: Int = 1,
  singlePort: Boolean = false,
  shouldReset: Boolean = false, extraReset: Boolean = false,
  holdRead: Boolean = false, bypassWrite: Boolean = false,
  conflictBehavior: SRAMConflictBehavior = DefaultBehavior,
  useBitmask: Boolean = false, withClockGate: Boolean = false,
  separateGateClock: Boolean = false,
  hasMbist: Boolean = false, latency: Int = 1, extraHold:Boolean = false,
  extClockGate: Boolean = false, hasSramCtl: Boolean = false,
  suffix: Option[String] = None
)(implicit valName: sourcecode.FullName) extends Module {
  val io = IO(new Bundle() {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way, useBitmask))
  })

  val extra_reset = if (extraReset) Some(IO(Input(Bool()))) else None

  require(set % setSplit == 0, "sets must be divisible by setSplit")
  val innerSets = set / setSplit
  val bankBits = log2Ceil(setSplit)
  val innerSetBits = log2Up(set) - bankBits
  val r_setIdx = io.r.req.bits.setIdx.head(innerSetBits)
  val r_bankSel = if(setSplit == 1) 0.U else io.r.req.bits.setIdx(bankBits - 1, 0)
  val w_setIdx = io.w.req.bits.setIdx.head(innerSetBits)
  val w_bankSel = if(setSplit == 1) 0.U else io.w.req.bits.setIdx(bankBits - 1, 0)

  require(way % waySplit == 0, "ways must be divisible by waySplit")
  val innerWays = way / waySplit

  require(gen.getWidth % dataSplit == 0, "data width must be divisible by dataSplit")
  val innerWidth = gen.getWidth / dataSplit

  val array = Seq.fill(setSplit)(Seq.fill(waySplit)(Seq.fill(dataSplit)(
    Module(new SRAMTemplate(UInt(innerWidth.W), innerSets, innerWays, singlePort,
    shouldReset, extraReset,
    holdRead, bypassWrite, conflictBehavior,
    useBitmask,withClockGate,
    separateGateClock, hasMbist, latency, extraHold,
    extClockGate, hasSramCtl,
    Some(suffix.getOrElse(SramHelper.getSramSuffix(valName.value)))
    ))
  )))

  for (i <- 0 until setSplit) {
    val ren = i.U === r_bankSel
    val wen = i.U === w_bankSel

    for (j <- 0 until waySplit) {
      val waymask = if (way > 1) io.w.req.bits.waymask.get(innerWays * (j+1) - 1, innerWays * j) else 1.U
      // if waymask-part is 0.U, we need not set array(i)(j)(_).io.w.req
      // TODO: consider whether to add this, which may lower power consumption, but will add burden to timing
      // val needWrite = waymask.orR

      for (k <- 0 until dataSplit) {
        if (array(i)(j)(k).extra_reset.isDefined) {
          array(i)(j)(k).extra_reset.get := extra_reset.get
        }
        array(i)(j)(k).io.r.req.bits.apply(r_setIdx)
        array(i)(j)(k).io.r.req.valid := io.r.req.valid && ren
        array(i)(j)(k).io.w.req.valid := io.w.req.valid && wen // && needWrite
        if (useBitmask) {
          array(i)(j)(k).io.w.req.bits.apply(
            VecInit(io.w.req.bits.data.slice(innerWays * j, innerWays * (j+1)).map(_.asUInt(innerWidth * (k+1) - 1, innerWidth * k))),
            w_setIdx, waymask, io.w.req.bits.bitmask.map(_.asUInt(innerWidth * (k+1) - 1, innerWidth * k)).get
          )
        } else {
          array(i)(j)(k).io.w.req.bits.apply(
            VecInit(io.w.req.bits.data.slice(innerWays * j, innerWays * (j+1)).map(_.asUInt(innerWidth * (k+1) - 1, innerWidth * k))),
            w_setIdx, waymask
          )
        }

      }
    }
  }

  val ren_vec_0 = VecInit((0 until setSplit).map(i => i.U === r_bankSel))
  val ren_vec_1 = RegEnable(ren_vec_0, 0.U.asTypeOf(ren_vec_0), io.r.req.valid)
  val ren_vec = ren_vec_1

  // only one read/write
  assert({PopCount(ren_vec) <= 1.U})

  // TODO: we should consider the readys of all sram to be accessed, and orR them
  // but since waySplitted and dataSplitted smaller srams should have the same behavior
  // we just use one of them for ready, for better timing
  io.r.req.ready := VecInit((0 until setSplit).map(i => array(i).head.head.io.r.req.ready))(r_bankSel)
  io.w.req.ready := VecInit((0 until setSplit).map(i => array(i).head.head.io.w.req.ready))(w_bankSel)


  // * an example of "setSplit 2, waySplit 2, dataSplit 4" of an SRAM with way 2 *
  // =========================================================================================
  //                               / way 0  -- [data 3] | [data 2] | [data 1] | [data 0]
  //  set[0] == 0.U -> waySplit 0 |- way 1  -- [data 3] | [data 2] | [data 1] | [data 0]
  // -----------------------------------------------------------------------------------------
  //                   waySplit 1 |- way 0  -- [data 3] | [data 2] | [data 1] | [data 0]
  //                               \ way 1  -- [data 3] | [data 2] | [data 1] | [data 0]
  // =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  //                               / way 0  -- [data 3] | [data 2] | [data 1] | [data 0]
  //  set[0] == 0.U -> waySplit 0 |- way 1  -- [data 3] | [data 2] | [data 1] | [data 0]
  // -----------------------------------------------------------------------------------------
  //                   waySplit 1 |- way 0  -- [data 3] | [data 2] | [data 1] | [data 0]
  //                               \ way 1  -- [data 3] | [data 2] | [data 1] | [data 0]
  // =========================================================================================
  // 1. aggregate data of the same line first
  // 2. collect all data lines in the same `WaySplit`
  // 3. use flatMap to collect all `WaySplit`, and we can get the targetData (Vec[T])
  // 4. use ren_vec to select the certain set
  val allData = (0 until setSplit).map(i =>
    VecInit((0 until waySplit).flatMap(j =>
      (0 until innerWays).map(w =>
        Cat((0 until dataSplit).map(k => array(i)(j)(k).io.r.resp.data(w).asUInt).reverse)
      )
    ))
  )

  io.r.resp.data := Mux1H(ren_vec, allData).asTypeOf(Vec(way, gen))
}

class FoldedSRAMTemplate[T <: Data](
  gen: T, set: Int, width: Int = 4, way: Int = 1,
  setSplit: Int = 1, waySplit: Int = 1, dataSplit: Int = 1,
  shouldReset: Boolean = false, extraReset: Boolean = false,
  holdRead: Boolean = false, singlePort: Boolean = false, bypassWrite: Boolean = false,
  conflictBehavior: SRAMConflictBehavior = DefaultBehavior, useBitmask: Boolean = false,
  withClockGate: Boolean = false,
  separateGateClock: Boolean = false, // no effect, only supports independent RW cg, only for API compatibility
  hasMbist: Boolean = false, latency: Int = 1,
  hasSramCtl: Boolean = false, suffix: Option[String] = None
)(implicit valName: sourcecode.FullName) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way, useBitmask))
  })
  val extra_reset = if (extraReset) Some(IO(Input(Bool()))) else None
  //   |<----- setIdx ----->|
  //   | ridx | width | way |

  require(width > 0 && isPow2(width))
  require(way > 0 && isPow2(way))
  require(set % width == 0)

  val nRows = set / width

  val array = Module(new SplittedSRAMTemplate(gen, set=nRows, way=width*way,
    setSplit=setSplit, waySplit=waySplit, dataSplit=dataSplit,
    shouldReset=shouldReset, extraReset=extraReset, holdRead=holdRead,
    singlePort=singlePort, bypassWrite=bypassWrite, conflictBehavior=conflictBehavior, useBitmask=useBitmask,
    withClockGate=withClockGate, separateGateClock=separateGateClock,
    hasMbist=hasMbist, latency=latency, extraHold = false, hasSramCtl = hasSramCtl,
    suffix = Some(suffix.getOrElse(SramHelper.getSramSuffix(valName.value)))))
  if (array.extra_reset.isDefined) {
    array.extra_reset.get := extra_reset.get
  }

  io.r.req.ready := array.io.r.req.ready
  io.w.req.ready := array.io.w.req.ready

  val raddr = io.r.req.bits.setIdx >> log2Ceil(width)
  val ridx = RegEnable(if (width != 1) io.r.req.bits.setIdx(log2Ceil(width)-1, 0) else 0.U(1.W), io.r.req.valid)
  val ren  = io.r.req.valid

  array.io.r.req.valid := ren
  array.io.r.req.bits.setIdx := raddr

  val rdata = array.io.r.resp.data
  for (w <- 0 until way) {
    val wayData = VecInit(rdata.indices.filter(_ % way == w).map(rdata(_)))
    val holdRidx = HoldUnless(ridx, GatedValidRegNext(io.r.req.valid))
    val realRidx = if (holdRead) holdRidx else ridx
    io.r.resp.data(w) := Mux1H(UIntToOH(realRidx, width), wayData)
  }

  val wen = io.w.req.valid
  val wdata = VecInit(Seq.fill(width)(io.w.req.bits.data).flatten)
  val waddr = io.w.req.bits.setIdx >> log2Ceil(width)
  val widthIdx = if (width != 1) io.w.req.bits.setIdx(log2Ceil(width)-1, 0) else 0.U
  val wmask = (width, way) match {
    case (1, 1) => 1.U(1.W)
    case (x, 1) => UIntToOH(widthIdx)
    case _      => VecInit(Seq.tabulate(width*way)(n => (n / way).U === widthIdx && io.w.req.bits.waymask.get(n % way))).asUInt
  }
  require(wmask.getWidth == way*width)

  if (useBitmask) {
    array.io.w.apply(wen, wdata, waddr, wmask, io.w.req.bits.bitmask.get)
  } else {
    array.io.w.apply(wen, wdata, waddr, wmask)
  }
}
class SRAMTemplateWithArbiter[T <: Data](nRead: Int, gen: T, set: Int, way: Int = 1,
  shouldReset: Boolean = false, hasMbist:Boolean = false, latency:Int = 1,
  hasSramCtl: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(Vec(nRead, new SRAMReadBus(gen, set, way)))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  val ram = Module(new SRAMTemplate(gen, set, way, shouldReset = shouldReset, holdRead = false, singlePort = true,
    hasMbist = hasMbist, latency = latency, hasSramCtl = hasSramCtl))
  ram.io.w <> io.w

  val readArb = Module(new Arbiter(chiselTypeOf(io.r(0).req.bits), nRead))
  readArb.io.in <> io.r.map(_.req)
  ram.io.r.req <> readArb.io.out

  // latch read results
  io.r.map{ case r => {
    r.resp.data := HoldUnless(ram.io.r.resp.data, GatedValidRegNext(r.req.fire))
  }}
}
