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

import chisel3._
import chisel3.util._
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

class SRAMTemplate[T <: Data](
  gen: T, set: Int, way: Int = 1, singlePort: Boolean = false,
  shouldReset: Boolean = false, extraReset: Boolean = false,
  holdRead: Boolean = false, bypassWrite: Boolean = false,
  useBitmask: Boolean = false, withClockGate: Boolean = false,
  separateGateClock: Boolean = false, // no effect, only supports independent RW cg, only for API compatibility
  hasMbist: Boolean = false, latency: Int = 1, extraHold:Boolean = false,
  extClockGate: Boolean = false
) extends Module {
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
  private val rclk = if(extClockGate) io.mbistCgCtl.get.rclk else rcg.map(_.out_clock).getOrElse(clock)
  private val wclk = if(extClockGate) io.mbistCgCtl.get.wclk else wcg.map(_.out_clock).getOrElse(clock)
  private val (mbistBd, array, vname) = SramHelper.genRam(
    sp = sp,
    set = set,
    dp = !singlePort,
    setup = 1,
    hold = if(extraHold) 1 else 0,
    latency = latency,
    bist = hasMbist,
    broadcast = io.broadcast,
    rclk,
    Some(wclk),
    suffix = "",
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

  private val (ren, wen) = (io.r.req.fire, io.w.req.valid || resetState)

  private val _wmask = if (useBitmask) {
    io.w.req.bits.flattened_bitmask.getOrElse("b1".U)
  } else {
    io.w.req.bits.waymask.getOrElse("b1".U)
  }
  private val wmask = Mux(resetState, Fill(arrayPortSize, true.B), _wmask)
  private val wdata = Mux(resetState, 0.U, io.w.req.bits.data.asUInt)
  private val waddr = Mux(resetState, resetSet, io.w.req.bits.setIdx)
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
  when(ramWen && !brcBd.ram_hold) {
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

  private val raw_rdata = ramRdata.asTypeOf(Vec(way, wordType))
  require(wdata.getWidth == ramRdata.getWidth)

  // bypass for dual-port SRAMs
  require(!bypassWrite || bypassWrite && !singlePort)
  def need_bypass(wen: Bool, waddr: UInt, wmask: UInt, ren: Bool, raddr: UInt) : UInt = {
    val need_check = ren && wen
    val need_check_reg = GatedValidRegNext(need_check)
    val waddr_reg = RegEnable(waddr, need_check)
    val raddr_reg = RegEnable(raddr, need_check)
    val wmask_reg = RegEnable(wmask, need_check)
    require(wmask.getWidth == way)
    val bypass = Fill(way, need_check_reg && waddr_reg === raddr_reg) & wmask_reg
    bypass.asTypeOf(UInt(way.W))
  }
  val bypass_wdata = if (bypassWrite) VecInit(RegEnable(io.w.req.bits.data, io.w.req.valid && io.r.req.valid).map(_.asTypeOf(wordType)))
    else VecInit((0 until way).map(_ => LFSR64().asTypeOf(wordType)))
  val bypass_mask = need_bypass(io.w.req.valid, io.w.req.bits.setIdx, io.w.req.bits.waymask.getOrElse("b1".U), io.r.req.valid, io.r.req.bits.setIdx)
  val mem_rdata = {
    if (singlePort) raw_rdata
    else VecInit(bypass_mask.asBools.zip(raw_rdata).zip(bypass_wdata).map {
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
  io.r.req.ready := !resetHold && !singleHold
  io.w.req.ready := !resetHold
}

class FoldedSRAMTemplate[T <: Data](
  gen: T, set: Int, width: Int = 4, way: Int = 1,
  shouldReset: Boolean = false, extraReset: Boolean = false,
  holdRead: Boolean = false, singlePort: Boolean = false,
  bypassWrite: Boolean = false, useBitmask: Boolean = false,
  withClockGate: Boolean = false, avoidSameAddr: Boolean = false,
  separateGateClock: Boolean = false, // no effect, only supports independent RW cg, only for API compatibility
  hasMbist: Boolean = false, latency: Int = 1
) extends Module {
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

  val array = Module(new SRAMTemplate(gen, set=nRows, way=width*way,
    shouldReset=shouldReset, extraReset=extraReset, holdRead=holdRead,
    singlePort=singlePort, bypassWrite=bypassWrite, useBitmask=useBitmask,
    withClockGate=withClockGate, separateGateClock=separateGateClock,
    hasMbist=hasMbist, latency=latency, extraHold = false))
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

  //Double port read and write to the same address to avoid
  val w_req      = io.w.req.valid
  val w_data     = io.w.req.bits.data
  val w_addr     = io.w.req.bits.setIdx >> log2Ceil(width)
  val w_widthIdx = if (width != 1) io.w.req.bits.setIdx(log2Ceil(width)-1, 0) else 0.U
  val w_waymask  = if(way > 1 ) io.w.req.bits.waymask.get else 0.U

  val conflict_valid    = WireInit(false.B)
  val conflict_addr     = WireInit(0.U.asTypeOf(w_addr))
  val conflict_data     = WireInit(0.U.asTypeOf(w_data))
  val conflict_widthIdx = WireInit(0.U.asTypeOf(w_widthIdx))
  val conflict_waymask  = WireInit(0.U.asTypeOf(w_waymask))

  val write_conflict    = w_req && ren && w_addr === raddr
  val can_write         = conflict_valid && (conflict_addr =/= raddr || !ren)
  val use_conflict_data = conflict_valid && conflict_addr === RegEnable(raddr, io.r.req.valid) && conflict_widthIdx === ridx

  if (!singlePort && avoidSameAddr) {
    //Cache write port data and wait for addresses to not conflict
    val b_valid    = RegInit(false.B)
    val b_addr     = RegInit(0.U.asTypeOf(w_addr))
    val b_data     = RegInit(0.U.asTypeOf(w_data))
    val b_widthIdx = RegInit(0.U.asTypeOf(w_widthIdx))
    val b_waymask  = RegInit(0.U.asTypeOf(w_waymask))
    when(write_conflict) {
      b_valid    := true.B
      b_addr     := w_addr
      b_data     := w_data
      b_widthIdx := w_widthIdx
      b_waymask  := w_waymask
    }
    when(can_write) {
      b_valid := false.B
    }
    conflict_valid    := b_valid
    conflict_addr     := b_addr
    conflict_data     := b_data
    conflict_widthIdx := b_widthIdx
    conflict_waymask  := b_waymask
  }

  val resp_data = WireInit(0.U.asTypeOf(io.r.resp.data))
  val rdata = array.io.r.resp.data
  for (w <- 0 until way) {
    val wayData = VecInit(rdata.indices.filter(_ % way == w).map(rdata(_)))
    val holdRidx = HoldUnless(ridx, GatedValidRegNext(io.r.req.valid))
    val realRidx = if (holdRead) holdRidx else ridx
    resp_data(w) := Mux1H(UIntToOH(realRidx, width), wayData)
  }
  io.r.resp.data := Mux(use_conflict_data, conflict_data, resp_data)

  val wen = if (!singlePort && avoidSameAddr) (io.w.req.valid && !write_conflict) || can_write else w_req
  val waddr = if (!singlePort && avoidSameAddr) Mux(conflict_valid, conflict_addr, w_addr) else w_addr
  val wdata = VecInit(Seq.fill(width)(
    if (!singlePort && avoidSameAddr) Mux(conflict_valid, conflict_data, w_data)
    else w_data
  ).flatten)
  val widthIdx = if (!singlePort && avoidSameAddr) Mux(conflict_valid, conflict_widthIdx, w_widthIdx) else w_widthIdx
  val waymask = if (!singlePort && avoidSameAddr) Mux(conflict_valid, conflict_waymask, w_waymask) else w_waymask
  val wmask = (width, way) match {
    case (1, 1) => 1.U(1.W)
    case (x, 1) => UIntToOH(widthIdx)
    case _      => VecInit(Seq.tabulate(width*way)(n => (n / way).U === widthIdx && waymask(n % way))).asUInt
  }
  require(wmask.getWidth == way*width)

  if (useBitmask) {
    array.io.w.apply(wen, wdata, waddr, wmask, io.w.req.bits.bitmask.get)
  } else {
    array.io.w.apply(wen, wdata, waddr, wmask)
  }
}
class SRAMTemplateWithArbiter[T <: Data](nRead: Int, gen: T, set: Int, way: Int = 1,
  shouldReset: Boolean = false, hasMbist:Boolean = false, latency:Int = 1) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(Vec(nRead, new SRAMReadBus(gen, set, way)))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  val ram = Module(new SRAMTemplate(gen, set, way, shouldReset = shouldReset, holdRead = false, singlePort = true,
    hasMbist = hasMbist, latency = latency))
  ram.io.w <> io.w

  val readArb = Module(new Arbiter(chiselTypeOf(io.r(0).req.bits), nRead))
  readArb.io.in <> io.r.map(_.req)
  ram.io.r.req <> readArb.io.out

  // latch read results
  io.r.map{ case r => {
    r.resp.data := HoldUnless(ram.io.r.resp.data, GatedValidRegNext(r.req.fire))
  }}
}
