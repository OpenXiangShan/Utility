/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  * *************************************************************************************
  */

/** ************************************************************************************
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
  * *************************************************************************************
  */

package utility

import chisel3._
import chisel3.util._
import sram._
import utility.mbist.MbistClockGateCell

class SRAMBundleA(val set: Int) extends Bundle {
  val setIdx = Output(UInt(log2Up(set).W))

  def apply(setIdx: UInt) = {
    this.setIdx := setIdx
    this
  }
}

class SRAMBundleAW[T <: Data](private val gen: T, set: Int, val way: Int = 1, val useBitmask: Boolean = false)
    extends SRAMBundleA(set) {

  private val dataWidth = gen.getWidth

  val data:    Vec[T] = Output(Vec(way, gen))
  val waymask: Option[UInt] = if (way > 1) Some(Output(UInt(way.W))) else None
  // flattened_bitmask is the flattened form of [waymask, bitmask], can be use directly to mask memory
  val flattened_bitmask: Option[UInt] = if (useBitmask) Some(Output(UInt((way * dataWidth).W))) else None
  // bitmask is the original bitmask passed from parameter
  val bitmask: Option[UInt] = if (useBitmask) Some(Output(UInt((dataWidth).W))) else None

  def apply(data: Vec[T], setIdx: UInt, waymask: UInt): SRAMBundleAW[T] = {
    require(
      waymask.getWidth == way,
      s"waymask width does not equal nWays, waymask width: ${waymask.getWidth}, nWays: ${way}"
    )
    super.apply(setIdx)
    this.data := data
    this.waymask.foreach(_ := waymask)
    this
  }

  def apply(data: Vec[T], setIdx: UInt, waymask: UInt, bitmask: UInt): SRAMBundleAW[T] = {
    require(useBitmask, "passing bitmask when not using bitmask")
    require(
      bitmask.getWidth == dataWidth,
      s"bitmask width does not equal data width, bitmask width: ${bitmask.getWidth}, data width: ${dataWidth}"
    )
    apply(data, setIdx, waymask)
    this.flattened_bitmask.foreach(
      _ :=
        VecInit.tabulate(way * dataWidth)(n => waymask(n / dataWidth) && bitmask(n % dataWidth)).asUInt
    )
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
  private val gen: T,
  val set:         Int,
  val way:         Int = 1,
  val useBitmask:  Boolean = false)
    extends Bundle {
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
/** Class for SRAM implementing
 * @param gen set the entry type
 * @param set set the array depth
 * @param way set the array way
 * @param singlePort set the SRAM to be single port. A Single port SRAM can not be written and read at the same time.
 * @param shouldReset set the SRAM to be resetable. A resetable SRAM will be clear to all zero after reset is deasserted.
 * @param extraReset add an extra reset port for the SRAM. Not applicable when [[shouldReset]] is false
 * @param holdRead store read value in an hold reg. Output data will use the value of the reg when no read is being served.
 * @param bypassWrite bypass the write to read when read and write with the same address are concurrent. Not applicable in single port SRAM.
 * @param useBitmask make one mask bit for every data bit.
 * @param multicycle read data will not be valid until [[multicycle]] cycle after the read request is received. Contact your leader before enable this.
 * @param holdMcp suppport for input hold multicycle, new port will be added. Contact your leader before enable this.
 * @param hasMbist enable MBIST support.
 * @param suffix add suffix in the name of SRAM wrapper
 * @param foundry set SRAM foundry name. Not applicable when [[hasMbist]] is false.
 * @param sramInst set SRAM inst name. Not applicable when [[hasMbist]] is false.
 */

class SRAMTemplate[T <: Data](
  gen:         T,
  set:         Int,
  way:         Int = 1,
  singlePort:  Boolean = false,
  shouldReset: Boolean = false,
  extraReset:  Boolean = false,
  holdRead:    Boolean = false,
  bypassWrite: Boolean = false,
  useBitmask:  Boolean = false,
  multicycle:  Int = 1,
  holdMcp:     Boolean = false,
  hasMbist:    Boolean = false,
  suffix:      String = "",
  foundry:     String = "Unknown",
  sramInst:    String = "STANDARD")
    extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val earlyRen = if(holdMcp) Some(Input(Bool())) else None
    val w = Flipped(new SRAMWriteBus(gen, set, way, useBitmask))
    val earlyWen = if(holdMcp) Some(Input(Bool())) else None
  })
  private val mcp = multicycle > 1
  private val cg = Module(new MbistClockGateCell(mcp))
  private val actualWay = if (useBitmask) gen.getWidth * way else way
  private val dataWidth = gen.getWidth * way
  require(multicycle >= 1)
  private val elementWidth = if (useBitmask) 1 else gen.getWidth
  private val (mbistBd, brcBd, array, nodeNum, realMaskBits) = SramHelper.genRam(
    elementWidth,
    actualWay,
    set,
    !singlePort,
    mcp,
    hasMbist,
    cg.out_clock,
    None,
    suffix,
    foundry,
    sramInst,
    this.asInstanceOf[SRAMTemplate[Data]]
  )

  val extra_reset = if (extraReset) Some(IO(Input(Bool()))) else None

  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  require(multicycle == 1 & shouldReset || !shouldReset, "MCP rams do not support reset!")
  if (shouldReset) {
    withClockAndReset(cg.out_clock, reset) {
      val _resetState = RegInit(true.B)
      val (_resetSet, resetFinish) = Counter(_resetState, set)
      when(resetFinish) {
        _resetState := false.B
      }
      if (extra_reset.isDefined) {
        when(extra_reset.get) {
          _resetState := true.B
        }
      }
      resetState := _resetState
      resetSet := _resetSet
    }
  }
  private val resetReg = RegNext(false.B, true.B)
  private val wen = io.w.req.fire || resetState && !resetReg
  private val _wmask =
    if (useBitmask) io.w.req.bits.flattened_bitmask.getOrElse("b1".U) else io.w.req.bits.waymask.getOrElse("b1".U)
  private val wmask = Mux(resetState, Fill(actualWay, true.B), _wmask)
  private val wdata = Mux(resetState, 0.U, io.w.req.bits.data.asUInt)
  private val waddr = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  private val ren = io.r.req.fire
  private val raddr = io.r.req.bits.setIdx

  private val ramWen = if (hasMbist) Mux(mbistBd.ack, mbistBd.we, wen) else wen
  private val ramWaddr = if (hasMbist & singlePort) {
    Mux(mbistBd.ack, mbistBd.addr_rd, waddr)
  } else if (hasMbist & !singlePort) {
    Mux(mbistBd.ack, mbistBd.addr, waddr)
  } else {
    waddr
  }

  private val nto1 = realMaskBits > way
  private val fullMbistMask = if(actualWay > 1) Fill(nodeNum, mbistBd.wmask) else Fill(realMaskBits, true.B)
  private val mbistWmask = if(nto1) {
    mbistBd.selectedOH & fullMbistMask
  } else {
    val n = realMaskBits / nodeNum
    val selMask = Cat(Seq.tabulate(realMaskBits)(i => mbistBd.selectedOH(i / n)).reverse)
    selMask & fullMbistMask
  }

  private val funcWmask = if(nto1) {
    val n = realMaskBits / way
    Cat(Seq.tabulate(realMaskBits)(i => wmask(i / n)).reverse)
  } else {
    wmask
  }

  private val mbistWdata = Fill(nodeNum, mbistBd.wdata)
  private val ramWmask = if (hasMbist) Mux(mbistBd.ack, mbistWmask, funcWmask) else funcWmask
  private val ramWdata = if (hasMbist) Mux(mbistBd.ack, mbistWdata, wdata) else wdata
  private val ramRen = if (hasMbist) Mux(mbistBd.ack, mbistBd.re, ren) else ren
  private val ramRaddr = if (hasMbist) Mux(mbistBd.ack, mbistBd.addr_rd, raddr) else raddr

  private val wenReg = RegInit(0.U(multicycle.W))
  private val renReg = RegInit(0.U(multicycle.W))
  wenReg := Cat(ramWen, wenReg) >> 1.U
  renReg := Cat(ramRen, renReg) >> 1.U

  private val wenStretched = if(holdMcp) {
    val ewe = if(hasMbist) mbistBd.ewe || io.earlyWen.get else io.earlyWen.get
    val wens = RegInit(0.U((multicycle - 1).W))
    when(ewe) {
      wens := Fill(multicycle - 1, true.B)
    }.otherwise {
      wens := Cat(ramWen, wens) >> 1.U
    }
    wens(0)
  } else {
    ramWen
  }

  private val renStretched = if(holdMcp) {
    val ere = if(hasMbist) mbistBd.ere || io.earlyRen.get else io.earlyRen.get
    val rens = RegInit(0.U((multicycle - 1).W))
    when(ere) {
      rens := Fill(multicycle - 1, true.B)
    }.otherwise {
      rens := Cat(ramRen, rens) >> 1.U
    }
    rens(0)
  } else {
    ramRen
  }

  private val ramRdata = SramProto.read(array, singlePort, ramRaddr, renStretched)
  when(wenStretched && !brcBd.ram_hold) {
    SramProto.write(array, singlePort, ramWaddr, ramWdata, ramWmask)
  }

  cg.dft.fromBroadcast(brcBd)
  cg.mbist.req := mbistBd.ack
  cg.mbist.readen := mbistBd.re
  cg.mbist.writeen := mbistBd.we
  cg.E := ren || wen

  private val concurrentRW = io.w.req.fire && io.r.req.fire && io.w.req.bits.setIdx === io.r.req.bits.setIdx
  private val doBypass = if (bypassWrite) concurrentRW else false.B
  private val doBypassReg = RegEnable(doBypass, false.B, io.r.req.fire)
  private val wmaskReg = RegEnable(wmask, 0.U, doBypass & io.r.req.fire)
  private val segment = dataWidth / wmask.getWidth
  private val bypassMask = Cat(Seq.tabulate(wmask.getWidth)(i => wmaskReg(i / segment).asBool).reverse)
  private val keepMask = Cat(Seq.tabulate(wmask.getWidth)(i => !wmaskReg(i / segment).asBool).reverse)
  private val rdataReg = Reg(UInt(dataWidth.W))
  private val bypassData = bypassMask & rdataReg | keepMask & ramRdata
  if(bypassWrite) {
    when(doBypass) {
      rdataReg := wdata.asUInt
    }.elsewhen(renReg(0)){
      rdataReg := Mux(doBypassReg, bypassData, ramRdata)
    }
  } else {
    when(renReg(0)) {
      rdataReg := ramRdata
    }
  }

  if(!bypassWrite && !holdRead) {
    io.r.resp.data := ramRdata.asTypeOf(io.r.resp.data)
  } else if(!bypassWrite && holdRead) {
    io.r.resp.data := Mux(renReg(0), ramRdata, rdataReg).asTypeOf(io.r.resp.data)
  } else if (bypassWrite && !holdRead) {
    io.r.resp.data := Mux(doBypassReg, bypassData, ramRdata).asTypeOf(io.r.resp.data)
  } else {
    when(renReg(0)){
      io.r.resp.data := Mux(doBypassReg, bypassData, ramRdata).asTypeOf(io.r.resp.data)
    }.otherwise {
      io.r.resp.data := rdataReg.asTypeOf(io.r.resp.data)
    }
  }
  private val mbistDw = dataWidth / nodeNum
  private val selectOHReg = RegEnable(mbistBd.selectedOH, renReg(0))
  mbistBd.rdata := Mux1H(selectOHReg, rdataReg.asTypeOf(Vec(nodeNum, UInt(mbistDw.W))))

  private val reqVector = renReg | wenReg
  private val singleHold = if (singlePort) io.w.req.valid else false.B
  private val mcpHold = if(mcp) reqVector(multicycle - 1, 1).orR else false.B
  private val resetHold = if(shouldReset) resetState || resetReg else false.B
  io.r.req.ready := !mcpHold && !resetHold && !singleHold
  io.w.req.ready := !mcpHold && !resetHold
  if(!bypassWrite) {
    assert(!concurrentRW, "Concurrent reading and writing to the same addr of SRAM is not allowed!")
  }
}

class FoldedSRAMTemplate[T <: Data](
  gen:         T,
  set:         Int,
  width:       Int = 4,
  way:         Int = 1,
  shouldReset: Boolean = false,
  extraReset:  Boolean = false,
  holdRead:    Boolean = false,
  singlePort:  Boolean = false,
  bypassWrite: Boolean = false,
  useBitmask:  Boolean = false,
  hasMbist : Boolean = false)
    extends Module {
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

  val array = Module(
    new SRAMTemplate(
      gen,
      set = nRows,
      way = width * way,
      shouldReset = shouldReset,
      extraReset = extraReset,
      holdRead = holdRead,
      singlePort = singlePort,
      bypassWrite = bypassWrite,
      useBitmask = useBitmask,
      hasMbist = hasMbist
    )
  )
  if (array.extra_reset.isDefined) {
    array.extra_reset.get := extra_reset.get
  }

  io.r.req.ready := array.io.r.req.ready
  io.w.req.ready := array.io.w.req.ready

  val raddr = io.r.req.bits.setIdx >> log2Ceil(width)
  val ridx = RegEnable(if (width != 1) io.r.req.bits.setIdx(log2Ceil(width) - 1, 0) else 0.U(1.W), io.r.req.valid)
  val ren = io.r.req.valid

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
  val widthIdx = if (width != 1) io.w.req.bits.setIdx(log2Ceil(width) - 1, 0) else 0.U
  val wmask = (width, way) match {
    case (1, 1) => 1.U(1.W)
    case (x, 1) => UIntToOH(widthIdx)
    case _ =>
      VecInit(Seq.tabulate(width * way)(n => (n / way).U === widthIdx && io.w.req.bits.waymask.get(n % way))).asUInt
  }
  require(wmask.getWidth == way * width)

  if (useBitmask) {
    array.io.w.apply(wen, wdata, waddr, wmask, io.w.req.bits.bitmask.get)
  } else {
    array.io.w.apply(wen, wdata, waddr, wmask)
  }
}

class SRAMTemplateWithArbiter[T <: Data](nRead: Int, gen: T, set: Int, way: Int = 1, shouldReset: Boolean = false)
    extends Module {
  val io = IO(new Bundle {
    val r = Flipped(Vec(nRead, new SRAMReadBus(gen, set, way)))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  val ram = Module(new SRAMTemplate(gen, set, way, shouldReset = shouldReset, holdRead = false, singlePort = true))
  ram.io.w <> io.w

  val readArb = Module(new Arbiter(chiselTypeOf(io.r(0).req.bits), nRead))
  readArb.io.in <> io.r.map(_.req)
  ram.io.r.req <> readArb.io.out

  // latch read results
  for (r <- io.r) {
    r.resp.data := HoldUnless(ram.io.r.resp.data, GatedValidRegNext(r.req.fire))
  }
}
