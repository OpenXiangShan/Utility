package utility.sram
import chisel3._
import chisel3.experimental.hierarchy.Instance
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import utility.SRAMTemplate
import utility.mbist.Mbist._
import utility.mbist.{Mbist, Ram2Mbist, Ram2MbistParams}

import scala.collection.mutable
import scala.math.sqrt

object SramHelper {
  private var nodeId = 0
  private var wrapperId = 0
  private var domainId = 0
  private val broadCastBdQueue = new mutable.Queue[SramBroadcastBundle]

  private def getWayNumForEachNodeAndNodeNum_1toN(dw: Int, way: Int, mw: Int): (Int, Int) = {
    val dataNum1toNNode = mw / dw
    val numVec = (1 to dataNum1toNNode)
    val validVec = numVec.map(num => (way % num == 0) && (way >= num))
    val validNum = numVec.zip(validVec).filter(_._2)
    val res = if (validNum.isEmpty) (1, way) else validNum.last
    (res._1, way / res._1)
  }

  private def getDivisor(in: Int): Seq[Int] = {
    val end = sqrt(in).toInt
    val divisors =
      Seq.tabulate(end)(_ + 1).map(idx => (in % idx == 0, Seq(idx, in / idx))).filter(_._1).flatMap(_._2).sorted
    divisors
  }

  private def getNodeNumForEachWayAndNodeNum_Nto1(dw: Int, way: Int, mw: Int): (Int, Int) = {
    val divisors = getDivisor(dw)
    val validDivisors = divisors.filter(_ <= mw)
    val goodNodeNumForEachWay = dw / validDivisors.max
    val defaultNodeNumForEachWay = ((dw + mw - 1) / mw)
    val finalNodeNumForEachWay =
      if (goodNodeNumForEachWay > 4 * defaultNodeNumForEachWay) defaultNodeNumForEachWay else goodNodeNumForEachWay
    (finalNodeNumForEachWay, way * finalNodeNumForEachWay)
  }

  def restartIndexing(): Unit = domainId = 0

  private def getDomainID(): Int = domainId

  private def increaseDomainID(add: Int): Unit = domainId += add

  def genBroadCastBundleTop(): SramBroadcastBundle = {
    val res = Wire(new SramBroadcastBundle)
    broadCastBdQueue.toSeq.foreach(bd => {
      BoringUtils.bore(bd) := res
    })
    broadCastBdQueue.clear()
    res
  }

  def genRam(
    ew:       Int,
    way:      Int,
    set:      Int,
    dp:       Boolean,
    mcp:      Boolean,
    bist:     Boolean,
    rclk:     Clock,
    wclk:     Option[Clock],
    suffix:   String,
    foundry:  String,
    sramInst: String,
    template: SRAMTemplate[Data]
  ): (Ram2Mbist, SramBroadcastBundle, Instance[SramArray], Int, Int) = {
    val isNto1 = ew > maxMbistDataWidth
    //** ******implement mbist interface node(multiple nodes for one way)******
    val (mbistNodeNumForEachWay, mbistNodeNumNto1) = getNodeNumForEachWayAndNodeNum_Nto1(ew, way, maxMbistDataWidth)
    val maskWidthNto1 = 1
    val mbistDataWidthNto1 = (ew + mbistNodeNumForEachWay - 1) / mbistNodeNumForEachWay
    //** *******implement mbist interface node(one node for multiple ways)******
    val (wayNumForEachNode, mbistNodeNum1toN) = getWayNumForEachNodeAndNodeNum_1toN(ew, way, maxMbistDataWidth)
    val mbistDataWidth1toN = wayNumForEachNode * ew
    val maskWidth1toN = wayNumForEachNode

    val mbistNodeNum = if (isNto1) mbistNodeNumNto1 else mbistNodeNum1toN
    val mbistDataWidth = if (isNto1) mbistDataWidthNto1 else mbistDataWidth1toN
    val mbistMaskWidth = if (isNto1) maskWidthNto1 else maskWidth1toN
    val mbistArrayIds = Seq.tabulate(mbistNodeNum)(idx => getDomainID() + idx)
    val bitWrite = way != 1
    val sramMaskBits = if(isNto1) mbistNodeNum else way

    val (array, vname) = SramProto(rclk, !dp, set, ew * way, sramMaskBits, mcp, wclk, bist, suffix)
    val bdParam =
      Ram2MbistParams(
        set,
        mbistDataWidth,
        mbistMaskWidth,
        !dp,
        vname,
        "",
        mbistNodeNum,
        mbistArrayIds.max,
        bitWrite,
        foundry,
        sramInst,
        0,
        "None",
        template
      )
    val mbist = if (bist) Some(IO(new Ram2Mbist(bdParam))) else None
    val mbistBundle = Wire(new Ram2Mbist(bdParam))
    mbistBundle := DontCare
    mbistBundle.selectedOH := Fill(mbistBundle.selectedOH.getWidth, 1.U(1.W))
    mbistBundle.ack := false.B
    mbistBundle.we := false.B
    mbistBundle.re := false.B
    mbistBundle.wmask := Fill(mbistMaskWidth, true.B)
    val broadCastSignals = Wire(new SramBroadcastBundle)
    broadCastSignals := DontCare
    if (bist) {
      dontTouch(mbist.get)
      mbist.get := DontCare
      mbist.get.suggestName("mbist")
      mbistBundle <> mbist.get
      Mbist.addRamNode(mbist.get, mbistArrayIds)
      val addId = if (isNto1) mbistNodeNumNto1 else mbistNodeNum1toN
      nodeId += addId
      increaseDomainID(addId)
      val broadcast = IO(new SramBroadcastBundle)
      broadcast := DontCare
      dontTouch(broadcast)
      broadcast.suggestName("broadcast")
      broadCastSignals := broadcast
      broadCastBdQueue.enqueue(broadcast)
      array.mbist.get.dft_ram_bp_clken := broadcast.ram_bp_clken
      array.mbist.get.dft_ram_bypass := broadcast.ram_bypass
    }
    (mbistBundle, broadCastSignals, array, mbistNodeNum, sramMaskBits)
  }
}
