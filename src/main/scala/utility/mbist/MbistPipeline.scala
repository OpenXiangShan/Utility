/** *************************************************************************************
 * Copyright (c) 2020-2022 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2022 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 * http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 * *************************************************************************************
 */

package utility.mbist

import chisel3._
import chisel3.util._
import utility.FileRegisters
import utility.mbist.Mbist._
import utility.mbist.MbistPipeline.uniqueId
import utility.sram.SramHelper

class MbitsStandardInterface(val params: MbistBusParams) extends Bundle {
  val array = Input(UInt(params.arrayWidth.W))
  val all, req = Input(Bool())
  val ack = Output(Bool())
  // write
  val writeen = Input(Bool())
  val be = Input(UInt(params.maskWidth.W))
  val addr = Input(UInt(params.addrWidth.W))
  val indata = Input(UInt(params.dataWidth.W))
  // read
  val readen = Input(Bool())
  val addr_rd = Input(UInt(params.addrWidth.W)) // not used for single port srams
  val outdata = Output(UInt(params.dataWidth.W))
}

case class InterfaceInfo(
  name: String,
  addrWidth: Int,
  dataWidth: Int,
  arrayWidth: Int,
  beWidth: Int,
  hasDualPort: Boolean) {
  override def toString = s"$name,$addrWidth,$dataWidth,$arrayWidth,$beWidth," + (if(hasDualPort) "true" else "false")
}

class MbistInterface(params: Seq[MbistBusParams], ids: Seq[Seq[Int]], name: String, pipelineNum: Int) extends Module {
  require(params.nonEmpty)
  require(params.length == pipelineNum, s"Error @ ${name}:Params Number and pipelineNum must be the same!")
  val myMbistBusParams = Mbist.inferMbistBusParamsFromParams(params)
  override val desiredName = name

  val toPipeline = IO(MixedVec(Seq.tabulate(pipelineNum)(idx => Flipped(new MbistBus(params(idx))))))
  val mbist = IO(new MbitsStandardInterface(myMbistBusParams))

  val info = InterfaceInfo(
    name,
    myMbistBusParams.addrWidth,
    myMbistBusParams.dataWidth,
    myMbistBusParams.arrayWidth,
    myMbistBusParams.maskWidth,
    myMbistBusParams.hasDualPort
  )

  private val array = mbist.array
  private val all = mbist.all
  private val req = mbist.req
  private val we = mbist.writeen
  private val be = mbist.be
  private val addr = mbist.addr
  private val inData = mbist.indata
  private val re = mbist.readen
  private val addrRd = mbist.addr_rd
  private val hit = if(params.length > 1) ids.map(item => item.map(_.U === array).reduce(_ | _)) else Seq(true.B)
  private val outDataVec = toPipeline.map(_.outdata)
  mbist.outdata := Mux1H(hit.zip(outDataVec))
  private val ackVec = toPipeline.map(_.ack)
  mbist.ack := Mux1H(hit.zip(ackVec))

  for(pip <- toPipeline) {
    pip.array := array
    pip.all := all
    pip.req := req
    pip.writeen := we
    pip.be := be
    pip.addr := addr
    pip.indata := inData
    pip.readen := re
    pip.addr_rd := addrRd
  }
}

object MbistPipeline {
  private var uniqueId = 0

  def PlaceMbistPipeline(
    level: Int,
    moduleName: String = s"MbistPipeline_${uniqueId}",
    place: Boolean = true
  ): Option[MbistPipeline] = {
    if(place) {
      val thisNode = Mbist.addController(level)
      uniqueId += 1
      val pipelineNodes =
        thisNode.children.filter(_.isInstanceOf[PipelineBaseNode]).map(_.asInstanceOf[PipelineBaseNode])
      val ramNodes = thisNode.children.filter(_.isInstanceOf[RamBaseNode]).map(_.asInstanceOf[RamBaseNode])
      val res = Module(new MbistPipeline(level, moduleName, thisNode))
      res.mbist.array := thisNode.bd.array
      res.mbist.all := thisNode.bd.all
      res.mbist.req := thisNode.bd.req
      thisNode.bd.ack := res.mbist.ack
      res.mbist.writeen := thisNode.bd.writeen
      res.mbist.be := thisNode.bd.be
      res.mbist.addr := thisNode.bd.addr
      res.mbist.indata := thisNode.bd.indata
      res.mbist.readen := thisNode.bd.readen
      res.mbist.addr_rd := thisNode.bd.addr_rd
      thisNode.bd.outdata := res.mbist.outdata

      res.toNextPipeline
        .zip(pipelineNodes)
        .foreach({
          case (a, b) =>
            b.bd.array := a.array
            b.bd.all := a.all
            b.bd.req := a.req
            a.ack := b.bd.ack
            b.bd.writeen := a.writeen
            b.bd.be := a.be
            b.bd.addr := a.addr
            b.bd.indata := a.indata
            b.bd.readen := a.readen
            b.bd.addr_rd := a.addr_rd
            a.outdata := b.bd.outdata
        })

      res.toSRAM
        .zip(ramNodes)
        .foreach({
          case (a, b) =>
            b.bd.addr := a.addr
            b.bd.addr_rd := a.addr_rd
            b.bd.wdata := a.wdata
            b.bd.wmask := a.wmask
            b.bd.re := a.re
            b.bd.we := a.we
            b.bd.ack := a.ack
            b.bd.selectedOH := a.selectedOH
            b.bd.array := a.array
            a.rdata := b.bd.rdata
        })
      Some(res)
    } else {
      None
    }
  }
}

class MbistPipeline(level: Int, moduleName: String = s"MbistPipeline_${uniqueId}", val myNode: PipelineBaseNode)
  extends Module {
  override val desiredName = moduleName

  def registerCSV(intf: InterfaceInfo, csvName: String): Unit = {
    val gen = new MbistCsvGen(intf, this, csvName)
    FileRegisters.add(s"$csvName.csv", gen.generate())
  }

  if(Mbist.isMaxLevel(level)) {
    //Within every mbist domain, sram arrays are indexed from 0
    SramHelper.restartIndexing()
  }
  val nodeParams = myNode.bd.params
  val childrenIds = myNode.children.flatMap(_.array_id)

  private val pipelineNodes =
    myNode.children.filter(_.isInstanceOf[PipelineBaseNode]).map(_.asInstanceOf[PipelineBaseNode])
  private val ramNodes = myNode.children.filter(_.isInstanceOf[RamBaseNode]).map(_.asInstanceOf[RamBaseNode])

  val mbist = IO(new MbistBus(myNode.bd.params))
  val toNextPipeline = pipelineNodes.map(_.bd.params).map(new MbistBus(_)).map(b => IO(Flipped(b)))
  val toSRAM = ramNodes.map(_.bd.params).map(new Ram2Mbist(_)).map(b => IO(Flipped(b)))
  dontTouch(mbist)
  toNextPipeline.foreach(b => dontTouch(b))
  toSRAM.foreach(b => dontTouch(b))

  private val arrayHit = myNode.array_id.map(_.U === mbist.array).reduce(_ | _)
  private val activated = mbist.all | (mbist.req & arrayHit)
  private val dataValid = activated & (mbist.readen | mbist.writeen)

  private val pipelineNodesAck =
    if(pipelineNodes.nonEmpty) toNextPipeline.map(_.ack).reduce(_ | _) else true.B

  private val arrayReg = RegEnable(mbist.array, 0.U, activated)
  private val reqReg = RegNext(mbist.req, 0.U)
  private val allReg = RegEnable(mbist.all, false.B, activated)
  mbist.ack := reqReg & pipelineNodesAck

  private val wenReg = RegEnable(mbist.writeen, 0.U, activated)
  private val beReg = RegEnable(mbist.be, 0.U, dataValid)
  private val addrReg = RegEnable(mbist.addr, 0.U, dataValid)
  private val dataInReg = RegEnable(mbist.indata, 0.U, dataValid)

  private val renReg = RegEnable(mbist.readen, 0.U, activated)
  private val addrRdReg = RegEnable(mbist.addr_rd, 0.U, dataValid)

  private val pipelineDataOut = Wire(Vec(toNextPipeline.length, mbist.outdata.cloneType))
  private val sramDataOut = Wire(Vec(toSRAM.length, mbist.outdata.cloneType))
  private val pipelineDataOutReg = RegEnable((pipelineDataOut :+ 0.U).reduce(_ | _), activated)
  private val sramDataOutReg = (sramDataOut :+ 0.U).reduce(_ | _)

  private def genHoldEnReg(sig:Bool):Option[UInt] = {
    val enReg = RegInit(0.U(2.W))
    when(activated && sig) {
      enReg := Fill(enReg.getWidth, true.B)
    }.elsewhen(enReg.orR) {
      enReg := Cat(false.B, enReg) >> 1.U
    }
    Some(enReg)
  }
  private val wenStretched = genHoldEnReg(mbist.writeen).get
  private val renStretched = genHoldEnReg(mbist.readen).get

  mbist.outdata := sramDataOutReg | pipelineDataOutReg

  ramNodes
    .zip(toSRAM)
    .zip(sramDataOut)
    .foreach({
      case ((child, bd), dout) =>
        val selectedVec = child.array_id.map(_.U === arrayReg)
        val selected = selectedVec.reduce(_ || _)
        val doSpread = selected || allReg
        val ren = if(bd.params.extraHold) renStretched(0) else renReg
        val wen = if(bd.params.extraHold) wenStretched(0) else wenReg
        bd.addr := Mux(doSpread, addrReg(child.bd.params.addrWidth - 1, 0), 0.U)
        bd.addr_rd := Mux(doSpread, addrRdReg(child.bd.params.addrWidth - 1, 0), 0.U)
        bd.wdata := dataInReg(child.bd.params.dataWidth - 1, 0)
        bd.re := Mux(doSpread, ren, 0.U)
        bd.we := Mux(doSpread, wen, 0.U)
        bd.wmask := beReg(child.bd.params.maskWidth - 1, 0)
        bd.ack := reqReg
        bd.selectedOH := Fill(selectedVec.length, allReg) | Mux(
          reqReg(0).asBool,
          Cat(selectedVec.reverse),
          ~0.U(child.bd.selectedOH.getWidth.W)
        ).asUInt
        bd.array := arrayReg
        dout := Mux(selected, bd.rdata, 0.U)
    })
  pipelineNodes
    .zip(toNextPipeline)
    .zip(pipelineDataOut)
    .foreach({
      case ((child, bd), dout) =>
        val selected = child.array_id.map(_.U === arrayReg).reduce(_ || _)
        val doSpread = selected || allReg
        bd.array := Mux(doSpread, arrayReg(child.bd.params.arrayWidth - 1, 0), 0.U)
        bd.req := reqReg
        bd.all := Mux(doSpread, allReg, 0.U)
        bd.writeen := Mux(doSpread, wenReg, 0.U)
        bd.be := beReg(child.bd.params.maskWidth - 1, 0)
        bd.addr := Mux(doSpread, addrReg(child.bd.params.addrWidth - 1, 0), 0.U)
        bd.indata := dataInReg(child.bd.params.dataWidth - 1, 0)
        bd.readen := Mux(doSpread, renReg, 0.U)
        bd.addr_rd := Mux(doSpread, addrRdReg(child.bd.params.addrWidth - 1, 0), 0.U)
        dout := Mux(selected, bd.outdata, 0.U)
    })
}
