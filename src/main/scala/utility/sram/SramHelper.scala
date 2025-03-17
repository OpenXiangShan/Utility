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
import chisel3.experimental.hierarchy.core.Instance
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import utility.mbist.Mbist._
import utility.mbist.{Mbist, Ram2Mbist, Ram2MbistParams}
import utility.sram.SramHelper._

import scala.collection.mutable
import scala.math.sqrt

case class SramInfo (
  dataBits:Int,
  way:Int,
  bist:Boolean
) {
  private val ew = dataBits
  private val isNto1 = ew > maxMbistDataWidth
  //** ******implement mbist interface node(multiple nodes for one way)******
  private val (mbistNodeNumForEachWay, mbistNodeNumNto1) = getNodeNumForEachWayAndNodeNum_Nto1(ew, way, maxMbistDataWidth)
  private val maskWidthNto1 = 1
  private val mbistDataWidthNto1 = (ew + mbistNodeNumForEachWay - 1) / mbistNodeNumForEachWay
  //** *******implement mbist interface node(one node for multiple ways)******
  private val (wayNumForEachNode, mbistNodeNum1toN) = getWayNumForEachNodeAndNodeNum_1toN(ew, way, maxMbistDataWidth)
  private val mbistDataWidth1toN = wayNumForEachNode * ew
  private val maskWidth1toN = wayNumForEachNode

  val mbistNodeNum = if(isNto1) mbistNodeNumNto1 else mbistNodeNum1toN
  val mbistDataWidth = if(isNto1) mbistDataWidthNto1 else mbistDataWidth1toN
  val mbistMaskWidth = if(isNto1) maskWidthNto1 else maskWidth1toN
  val mbistArrayIds = if(bist) Seq.tabulate(mbistNodeNum)(idx => getDomainID + idx) else Seq.fill(mbistNodeNum)(0)
  val bitWrite = way != 1
  val sramMaskBits = if(isNto1) mbistNodeNum else way
  val sramDataBits = way * dataBits
  if(bist) {
    val addId = if(isNto1) mbistNodeNumNto1 else mbistNodeNum1toN
    increaseNodeID(addId)
    increaseDomainID(addId)
  }
  def mbistMaskConverse(wmask:UInt, nodeSelectOH:UInt):UInt = {
    val fullMask = if(way > 1) Fill(mbistNodeNum, wmask) else Fill(mbistNodeNum, true.B)
    if(isNto1) {
      nodeSelectOH & fullMask
    } else {
      val n = sramMaskBits / mbistNodeNum
      val selMask = Cat(Seq.tabulate(sramMaskBits)(i => nodeSelectOH(i / n)).reverse)
      selMask & fullMask
    }
  }

  def funcMaskConverse(mask:UInt): UInt = {
    if(isNto1) {
      val n = sramMaskBits / way
      Cat(Seq.tabulate(sramMaskBits)(i => mask(i / n)).reverse)
    } else {
      mask
    }
  }
}

object SramHelper {
  private var nodeId = 0
  private var wrapperId = 0
  private var domainId = 0
  val broadCastBdQueue = new mutable.Queue[SramBroadcastBundle]

  def getWayNumForEachNodeAndNodeNum_1toN(dw: Int, way: Int, mw: Int): (Int, Int) = {
    val dataNum1toNNode = mw / dw
    val numVec = (1 to dataNum1toNNode)
    val validVec = numVec.map(num => (way % num == 0) && (way >= num))
    val validNum = numVec.zip(validVec).filter(_._2)
    val res = if(validNum.isEmpty) (1, way) else validNum.last
    (res._1, way / res._1)
  }

  private def getDivisor(in: Int): Seq[Int] = {
    val end = sqrt(in).toInt
    val divisors =
      Seq.tabulate(end)(_ + 1).map(idx => (in % idx == 0, Seq(idx, in / idx))).filter(_._1).flatMap(_._2).sorted
    divisors
  }

  def getNodeNumForEachWayAndNodeNum_Nto1(dw: Int, way: Int, mw: Int): (Int, Int) = {
    val divisors = getDivisor(dw)
    val validDivisors = divisors.filter(_ <= mw)
    val goodNodeNumForEachWay = dw / validDivisors.max
    val defaultNodeNumForEachWay = ((dw + mw - 1) / mw)
    val finalNodeNumForEachWay =
      if(goodNodeNumForEachWay > 4 * defaultNodeNumForEachWay) defaultNodeNumForEachWay else goodNodeNumForEachWay
    (finalNodeNumForEachWay, way * finalNodeNumForEachWay)
  }

  def restartIndexing(): Unit = domainId = 0

  def getDomainID: Int = domainId

  def increaseDomainID(add: Int): Unit = domainId += add

  def increaseNodeID(add: Int): Unit = nodeId += add

  def genBroadCastBundleTop(): SramBroadcastBundle = {
    val res = Wire(new SramBroadcastBundle)
    broadCastBdQueue.toSeq.foreach(bd => {
      BoringUtils.bore(bd) := res
    })
    broadCastBdQueue.clear()
    res
  }

  def genRam(
    sp: SramInfo,
    set: Int,
    dp: Boolean,
    setup: Int,
    hold: Int,
    latency: Int,
    bist: Boolean,
    broadcast: Option[SramBroadcastBundle],
    hasSramCtl: Boolean,
    rclk: Clock,
    wclk: Option[Clock],
    suffix: String,
    foundry: String,
    sramInst: String,
    template: RawModule
  ): (Ram2Mbist, Instance[SramArray], String) = {

    val (array, vname) = SramProto(rclk, !dp, set, sp.sramDataBits, sp.sramMaskBits, setup, hold, latency, wclk, bist || broadcast.isDefined, hasSramCtl || broadcast.isDefined, suffix)
    val bdParam = Ram2MbistParams(
      sp,
      set,
      !dp,
      vname,
      "",
      foundry,
      sramInst,
      hold == 1,
      0,
      "None",
      template
    )
    val mbist = Wire(new Ram2Mbist(bdParam))
    mbist := DontCare
    mbist.selectedOH := Fill(mbist.selectedOH.getWidth, 1.U(1.W))
    mbist.ack := false.B
    mbist.we := false.B
    mbist.re := false.B
    mbist.wmask := Fill(sp.mbistMaskWidth, true.B)
    if(broadcast.isDefined || bist) {
      array.mbist.get.dft_ram_bp_clken := broadcast.get.mbist.ram_bp_clken
      array.mbist.get.dft_ram_bypass := broadcast.get.mbist.ram_bypass
    }
    if(broadcast.isDefined || hasSramCtl) {
      array.sramCtl.get := broadcast.get.sramCtl
    }
    if(bist) {
      dontTouch(mbist)
      broadcast.get := DontCare
      dontTouch(broadcast.get)
      SramHelper.broadCastBdQueue.enqueue(broadcast.get)
      Mbist.addRamNode(mbist, sp.mbistArrayIds)
    }
    (mbist, array, vname)
  }

  private val shortMap = Seq(
    //l2 cache abbreviations
    "coupledL2" -> "l2",
    "DataStorage" -> "dat",
    "tagArray" -> "tag",
    "metaArray" -> "meta",
    "replacer_sram_opt" -> "repl",
    "origin_bit_opt" -> "orgb",
    "rrTable" -> "rrt",
    "tpMetaTable" -> "tp_meta",

    //icache abbreviations
    "icache" -> "icsh",
    "SRAMTemplateWithFixedWidth" -> "dat",

    //bp abbreviations
    "TageTable" -> "bp_tage",
    "TageBTable" -> "bp_tage",
    "SCTable" -> "bp_sc",
    "ITTageTable" -> "bp_ittage",
    "FTB" -> "bp_ftb",
    "us" -> "us",
    "bt" -> "bt",

    //ftq abbreviations
    "FtqNRSRAM" -> "ftq",

    //dcache abbreviations
    "DataSRAMBank" -> "dcsh_dat",
    "TagArray" -> "dcsh_tag",

    //mmu abbreviations
    "PtwCache" -> "ptw",
    "l0" -> "l0",
    "l1" -> "l1",

    //prefetch abbreviations
    "prefetch" -> "pftch",
    "pht_ram" -> "pht"
  ).toMap

  def getSramSuffix(vn:String):String = {
    val vns = vn.split("\\.").toSeq
    val vni = vns.init
    val vnl = vns.last
    val pfx = vni.filter(shortMap.contains).map(shortMap)
    val sfx = if(shortMap.contains(vnl)) shortMap(vnl) else ""
    val ns = (pfx :+ sfx).filterNot(_ == "")
    if(ns.nonEmpty) {
      ns.reduce((a:String, b:String) => s"${a}_${b}")
    } else {
      println(s"[Warning]: ${vn} dose not match any abbreviation!")
      ""
    }
  }
}
