/** *************************************************************************************
  * Copyright (c) 2020-2022 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2022 Peng Cheng Laboratory
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

package utility.mbist

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import utility.SRAMTemplate

trait MbistBundleLike {
  this: Bundle =>
  def sink_elms:   Seq[String]
  def source_elms: Seq[String]
  def get_sink_data:   Seq[Data] = sink_elms.map(e => elements(e))
  def get_source_data: Seq[Data] = sink_elms.map(e => elements(e))
}
abstract class MbistCommonBundle() extends Bundle with MbistBundleLike {
  def sink_elms:   Seq[String] = Seq()
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
  val mbist_array = Input(UInt(params.arrayWidth.W))
  val mbist_all, mbist_req = Input(Bool())
  val mbist_ack = Output(Bool())
  // write
  val mbist_writeen = Input(Bool())
  val mbist_be = Input(UInt(params.maskWidth.W))
  val mbist_addr = Input(UInt(params.addrWidth.W))
  val mbist_indata = Input(UInt(params.dataWidth.W))
  // read
  val mbist_readen = Input(Bool())
  val mbist_addr_rd = Input(UInt(params.addrWidth.W)) // not used for single port srams
  val mbist_outdata = Output(UInt(params.dataWidth.W))

  override def sink_elms: Seq[String] = super.sink_elms ++ Seq(
    "mbist_array",
    "mbist_all",
    "mbist_req",
    "mbist_writeen",
    "mbist_be",
    "mbist_addr",
    "mbist_indata",
    "mbist_readen",
    "mbist_addr_rd"
  )

  override def source_elms: Seq[String] = super.source_elms ++ Seq("mbist_ack", "mbist_outdata")
}

case class Ram2MbistParams(
  set:        Int,
  dataWidth:  Int,
  maskWidth:  Int,
  singlePort: Boolean,
  vname:      String,
  nodeSuffix: String,
  nodeNum:    Int,
  maxArrayId: Int,
  bitWrite:   Boolean,
  foundry:    String,
  sramInst:   String,
  latency:    Int = 0,
  bankRange:  String = "None",
  holder:     SRAMTemplate[Data]) {
  val addrWidth = log2Up(set + 1)
  val arrayWidth = log2Up(maxArrayId + 1)
  def getAllNodesParams(): Seq[Ram2MbistParams] = {
    val res = Seq.tabulate(nodeNum)(idx => {
      Ram2MbistParams(
        set,
        dataWidth,
        maskWidth,
        singlePort,
        vname,
        nodeSuffix + s"_node$idx",
        nodeNum,
        maxArrayId,
        bitWrite,
        foundry,
        sramInst,
        latency,
        bankRange,
        holder
      )
    })
    res
  }
}

class Ram2Mbist(val params: Ram2MbistParams) extends MbistCommonBundle() {
  val addr, addr_rd = Input(UInt(params.addrWidth.W))
  val wdata = Input(UInt(params.dataWidth.W))
  val wmask = Input(UInt(params.maskWidth.W))
  val re, we = Input(Bool())
  val ere, ewe = Input(Bool())
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
    "ere",
    "ewe",
    "ack",
    "selectedOH",
    "array"
  )
  override def source_elms: Seq[String] = super.source_elms ++ Seq("rdata")
}
