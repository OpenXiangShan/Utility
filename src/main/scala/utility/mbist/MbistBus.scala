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

package utility.mbist

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import utility.sram.{SRAMTemplate, SramInfo}

trait MbistBundleLike {
  this: Bundle =>
  def sink_elms: Seq[String]

  def source_elms: Seq[String]

  def get_sink_data: Seq[Data] = sink_elms.map(e => elements(e))

  def get_source_data: Seq[Data] = sink_elms.map(e => elements(e))
}

abstract class MbistCommonBundle() extends Bundle with MbistBundleLike {
  def sink_elms: Seq[String] = Seq()

  def source_elms: Seq[String] = Seq()
}

case class MbistBusParams(
  array:       Int,
  set:         Int,
  dataWidth:   Int,
  maskWidth:   Int,
  hasDualPort: Boolean,
  domainName:  String = "Unknown") {
  val arrayWidth = log2Up(array + 1)
  val addrWidth = log2Up(set + 1)
}

class MbistBus(val params: MbistBusParams) extends MbistCommonBundle() {
  // control signals
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

  override def sink_elms: Seq[String] = super.sink_elms ++ Seq(
    "array",
    "all",
    "req",
    "writeen",
    "be",
    "addr",
    "indata",
    "readen",
    "addr_rd"
  )

  override def source_elms: Seq[String] = super.source_elms ++ Seq("ack", "outdata")
}

case class Ram2MbistParams(
  sramParams: SramInfo,
  set:        Int,
  singlePort: Boolean,
  vname:      String,
  nodeSuffix: String,
  foundry:    String,
  sramInst:   String,
  extraHold:  Boolean,
  latency:    Int = 0,
  bankRange:  String = "None",
  holder:     RawModule) {
  val dataWidth = sramParams.mbistDataWidth
  val maskWidth = sramParams.mbistMaskWidth
  val maxArrayId = sramParams.mbistArrayIds.max
  val nodeNum = sramParams.mbistNodeNum
  val bitWrite = sramParams.bitWrite
  val addrWidth = log2Up(set + 1)
  val arrayWidth = log2Up(maxArrayId + 1)

  def getAllNodesParams: Seq[Ram2MbistParams] = {
    Seq.tabulate(nodeNum)(idx => this.copy(nodeSuffix = s"_node$idx"))
  }
}

class Ram2Mbist(val params: Ram2MbistParams) extends MbistCommonBundle() {
  val addr, addr_rd = Input(UInt(params.addrWidth.W))
  val wdata = Input(UInt(params.dataWidth.W))
  val wmask = Input(UInt(params.maskWidth.W))
  val re, we = Input(Bool())
  val rdata = Output(UInt(params.dataWidth.W))
  val ack = Input(Bool())
  val selectedOH = Input(UInt(params.nodeNum.W))
  val array = Input(UInt(params.arrayWidth.W))

  override def sink_elms: Seq[String] = super.sink_elms ++ Seq(
    "addr",
    "addr_rd",
    "wdata",
    "wmask",
    "re",
    "we",
    "ack",
    "selectedOH",
    "array"
  )

  override def source_elms: Seq[String] = super.source_elms ++ Seq("rdata")
}
