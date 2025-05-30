/***************************************************************************************
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
***************************************************************************************/

package utility.chiron

import chisel3._
import chisel3.util._

/*
* CHI Parameters container.
*/
class CHIParameters(
  val issue               : Int,
  val nodeIdWidth         : Int,
  val reqAddrWidth        : Int,
  val reqRsvdcWidth       : Int,
  val datRsvdcWidth       : Int,
  val dataWidth           : Int,
  val dataCheckPresent    : Boolean,
  val poisonPresent       : Boolean,
  val mpamPresent         : Boolean
);

/* 
* CLog.B parameters write operations.
*/
private object CLogBWriteParameters {
  var uniqueCounter = -1
  def first: Boolean = uniqueCounter == 0
  def nextUniqueIndex: Int = {
    uniqueCounter = uniqueCounter + 1
    uniqueCounter
  }
}

private class CLogBWriteParameters(id: String, params: CHIParameters) extends BlackBox with HasBlackBoxInline {

  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val en = Input(Bool())
  })

  val moduleName = s"CLogB_StubWriteParameters_${CLogBWriteParameters.nextUniqueIndex}"
  val dpicFunc = "CLogB_SharedWriteParameters"

  val verilog =
    s"""
    |module $moduleName (
    |  input   logic               clock,
    |  input   logic               reset,
    |  input   logic               en
    |);
    |
    |  reg singleton;
    |
    |  always @(posedge clock) begin
    |
    |    if (reset) begin
    |      singleton <= 1'b0;
    |    end
    |    else if (en && !reset && !singleton) begin
    |      singleton <= 1'b1;
    |      $dpicFunc(
    |        \"${id}\", 
    |        ${params.issue}, /* issue */
    |        ${params.nodeIdWidth}, /* nodeIdWidth */
    |        ${params.reqAddrWidth}, /* addrWidth */
    |        ${params.reqRsvdcWidth}, /* reqRsvdcWidth */
    |        ${params.datRsvdcWidth}, /* datRsvdcWidth */
    |        ${params.dataWidth}, /* dataWidth */
    |        ${if (params.dataCheckPresent) "1" else "0"}, /* dataCheckPresent */
    |        ${if (params.poisonPresent) "1" else "0"}, /* poisonPresent */
    |        ${if (params.mpamPresent) "1" else "0"} /* mpamPresent */
    |      );
    |    end
    |  end
    |
    |endmodule
    |""".stripMargin

  val verilogImport =
    s"""
    |import "DPI-C" function void $dpicFunc (
    |  input   string              id,
    |  input   int                 issue,
    |  input   int                 nodeIdWidth,
    |  input   int                 addrWidth,
    |  input   int                 reqRsvdcWidth,
    |  input   int                 datRsvdcWidth,
    |  input   int                 dataWidth,
    |  input   int                 dataCheckPresent,
    |  input   int                 poisonPresent,
    |  input   int                 mpamPresent
    |);
    |
    |""".stripMargin

  val verilogWithFirstImport = {
    if (CLogBWriteParameters.first)
      verilog + verilogImport
    else
      verilog
  }

  setInline(s"$moduleName.sv", verilogWithFirstImport)

  override def desiredName: String = moduleName
}
//

/*
* CLog.B topologies write operations.
*/
private object CLogBWriteTopo {
  var uniqueCounter = -1
  def first: Boolean = uniqueCounter == 0
  def nextUniqueIndex: Int = {
    uniqueCounter = uniqueCounter + 1
    uniqueCounter
  }
}

private class CLogBWriteTopo(id: String, topo: Seq[(Int, Int)], uniqueName: String = "", desc: String = "") extends BlackBox with HasBlackBoxInline {

  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val en = Input(Bool())
  })

  val suffixName = {
    if (uniqueName.isEmpty) 
      s"${CLogBWriteTopo.nextUniqueIndex}" 
    else 
      s"${CLogBWriteTopo.nextUniqueIndex}_$uniqueName"
  }

  val moduleName = s"CLogB_StubWriteTopo_${suffixName}_${desc}"
  val dpicFuncWrite = "CLogB_SharedWriteTopo"
  val dpicFuncEnd = "CLogB_SharedWriteTopoEnd"

  val verilog =
    s"""
    |/* $desc */
    |module $moduleName (
    |  input   logic               clock,
    |  input   logic               reset,
    |  input   logic               en
    |);
    |
    |  always @(posedge clock) begin
    |
    |    if (en && !reset) begin
    |${topo.map{ case (nid, typ) => s"      $dpicFuncWrite(\"${id}\", $nid, $typ);" }.mkString("\n")}
    |      $dpicFuncEnd(\"${id}\");
    |    end
    |  end
    |
    |endmodule
    |""".stripMargin

  val verilogImport =
    s"""
    |import "DPI-C" function void $dpicFuncWrite (
    |  input   string              id,
    |  input   int                 nodeId,
    |  input   int                 nodeType
    |);
    |
    |import "DPI-C" function void $dpicFuncEnd (
    |  input   string              id
    |);
    |""".stripMargin

  val verilogWithFirstImport = {
    if (CLogBWriteTopo.first)
      verilog + verilogImport
    else
      verilog
  }
    
  setInline(s"$moduleName.sv", verilogWithFirstImport)

  override def desiredName: String = moduleName
}
//

/* 
* CLog.B log record write operations. 
*/
private object CLogBWriteRecord {
  var uniqueCounter = -1
  def first: Boolean = uniqueCounter == 0
  def nextUniqueIndex: Int = {
    uniqueCounter = uniqueCounter + 1
    uniqueCounter
  }
}

private class CLogBWriteRecord(id: String, channel: Int, flitLength: Int, vTime: Boolean, uniqueName: String = "", desc: String = "") extends BlackBox with HasBlackBoxInline {

  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val en = Input(Bool())
    val cycleTime = Input(UInt(64.W))
    val nodeId = Input(UInt(32.W))
    val flit = Input(UInt(512.W))
  })

  val suffixName = {
    if (uniqueName.isEmpty) 
      s"${CLogBWriteRecord.nextUniqueIndex}" 
    else 
      s"${CLogBWriteRecord.nextUniqueIndex}_$uniqueName"
  }

  val moduleName = s"CLogB_StubWriteRecord_${suffixName}_${desc}"
  val dpicFunc = "CLogB_SharedWriteRecord"

  val time = if (vTime) "$time" else "cycleTime"

  val verilog =
    s"""
    |/* $desc */
    |module $moduleName (
    |  input   logic               clock,
    |  input   logic               reset,
    |  input   logic               en,
    |  input   longint             cycleTime,
    |  input   int                 nodeId,
    |  input   bit [511:0]         flit
    |);
    |
    |  always @(posedge clock) begin
    |
    |    if (en && !reset) begin
    |      $dpicFunc(\"${id}\", ${time}, nodeId, 32'd${channel}, flit, 32'd${flitLength});
    |    end
    |  end
    |
    |endmodule
    |""".stripMargin

  val verilogImport =
    s"""
    |import "DPI-C" function void $dpicFunc (
    |  input   string              id,
    |  input   longint             cycletime,
    |  input   int                 nodeId,
    |  input   int                 channel,
    |  input   bit [511:0]         flit,
    |  input   int                 flitLength
    |);
    |""".stripMargin

  val verilogWithFirstImport = {
    if (CLogBWriteRecord.first)
      verilog + verilogImport
    else
      verilog
  }

  setInline(s"$moduleName.sv", verilogWithFirstImport)

  override def desiredName: String = moduleName
}
//

object CLog {
        
  /* Issue value enumeration */
  val IssueB: Int = 0
  val IssueE: Int = 3

  /* Node type enumeration */
  val NodeTypeRNF: Int = 1
  val NodeTypeRND: Int = 2
  val NodeTypeRNI: Int = 3
  val NodeTypeHNF: Int = 5
  val NodeTypeHNI: Int = 7
  val NodeTypeSNF: Int = 9
  val NodeTypeSNI: Int = 11
  val NodeTypeMN: Int = 12

  /* Channel enumeration */
  val ChannelTXREQ: Int = 0
  val ChannelTXRSP: Int = 1
  val ChannelTXDAT: Int = 2
  val ChannelTXSNP: Int = 3
  val ChannelRXREQ: Int = 4
  val ChannelRXRSP: Int = 5
  val ChannelRXDAT: Int = 6
  val ChannelRXSNP: Int = 7
}

object CLogB {

  private var enable = false

  def init(enable: Boolean): Unit = {
    this.enable = enable
  }

  def logParameters(id: String, clock: Clock, reset: Reset, en: Bool, params: CHIParameters): Unit = {
    if (enable) {
      val stub = Module(new CLogBWriteParameters(id, params))
      stub.io.clock := clock
      stub.io.reset := reset
      stub.io.en := en
    }
  }

  def logTopology(id: String, clock: Clock, reset: Reset, en: Bool, topo: Seq[(Int, Int)]): Unit = {
    if (enable) {
      val stub = Module(new CLogBWriteTopo(id, topo))
      stub.io.clock := clock
      stub.io.reset := reset
      stub.io.en := en
    }
  }

  def logFlit(desc: String, id: String, channel: Int, vTime: Boolean, clock: Clock, reset: Reset, nodeId: UInt, flit: UInt, flitv: Bool, time: UInt = 0.U, timev: Bool = false.B): Unit = {
    if (enable) {
      val stub = Module(new CLogBWriteRecord(id, channel, flit.getWidth, vTime, "", desc))
      val cycle = RegInit(0.U(64.W))
      cycle := cycle + 1.U
      stub.io.clock := clock
      stub.io.reset := reset
      stub.io.en := flitv
      stub.io.cycleTime := Mux(timev, time, cycle)
      stub.io.nodeId := nodeId
      stub.io.flit := flit
    }
  }

  def logFlitTXREQ(id: String, vTime: Boolean, clock: Clock, reset: Reset, nodeId: UInt, flit: UInt, flitv: Bool, time: UInt = 0.U, timev: Bool = false.B): Unit = {
    logFlit("TXREQ", id, CLog.ChannelTXREQ, vTime, clock, reset, nodeId, flit, flitv, time, timev)
  }

  def logFlitTXRSP(id: String, vTime: Boolean, clock: Clock, reset: Reset, nodeId: UInt, flit: UInt, flitv: Bool, time: UInt = 0.U, timev: Bool = false.B): Unit = {
    logFlit("TXRSP", id, CLog.ChannelTXRSP, vTime, clock, reset, nodeId, flit, flitv, time, timev)
  }

  def logFlitTXDAT(id: String, vTime: Boolean, clock: Clock, reset: Reset, nodeId: UInt, flit: UInt, flitv: Bool, time: UInt = 0.U, timev: Bool = false.B): Unit = {
    logFlit("TXDAT", id, CLog.ChannelTXDAT, vTime, clock, reset, nodeId, flit, flitv, time, timev)
  }

  def logFlitTXSNP(id: String, vTime: Boolean, clock: Clock, reset: Reset, nodeId: UInt, flit: UInt, flitv: Bool, time: UInt = 0.U, timev: Bool = false.B): Unit = {
    logFlit("TXSNP", id, CLog.ChannelTXSNP, vTime, clock, reset, nodeId, flit, flitv, time, timev)
  }

  def logFlitRXREQ(id: String, vTime: Boolean, clock: Clock, reset: Reset, nodeId: UInt, flit: UInt, flitv: Bool, time: UInt = 0.U, timev: Bool = false.B): Unit = {
    logFlit("RXREQ", id, CLog.ChannelRXREQ, vTime, clock, reset, nodeId, flit, flitv, time, timev)
  }

  def logFlitRXRSP(id: String, vTime: Boolean, clock: Clock, reset: Reset, nodeId: UInt, flit: UInt, flitv: Bool, time: UInt = 0.U, timev: Bool = false.B): Unit = {
    logFlit("RXRSP", id, CLog.ChannelRXRSP, vTime, clock, reset, nodeId, flit, flitv, time, timev)
  }

  def logFlitRXDAT(id: String, vTime: Boolean, clock: Clock, reset: Reset, nodeId: UInt, flit: UInt, flitv: Bool, time: UInt = 0.U, timev: Bool = false.B): Unit = {
    logFlit("RXDAT", id, CLog.ChannelRXDAT, vTime, clock, reset, nodeId, flit, flitv, time, timev)
  }

  def logFlitRXSNP(id: String, vTime: Boolean, clock: Clock, reset: Reset, nodeId: UInt, flit: UInt, flitv: Bool, time: UInt = 0.U, timev: Bool = false.B): Unit = {
    logFlit("RXSNP", id, CLog.ChannelRXSNP, vTime, clock, reset, nodeId, flit, flitv, time, timev)
  }

  def logFlitsRNOfRNF(id          : String,
                      vTime     : Boolean,
                      clock       : Clock,
                      reset       : Reset,
                      rnId        : UInt,
                      txreqflit   : UInt,
                      txreqflitv  : Bool,
                      rxrspflit   : UInt,
                      rxrspflitv  : Bool,
                      rxdatflit   : UInt,
                      rxdatflitv  : Bool,
                      rxsnpflit   : UInt,
                      rxsnpflitv  : Bool,
                      txrspflit   : UInt,
                      txrspflitv  : Bool,
                      txdatflit   : UInt,
                      txdatflitv  : Bool,
                      time        : UInt = 0.U,
                      timev       : Bool = false.B): Unit = {
    logFlitTXREQ(id, vTime, clock, reset, rnId, txreqflit, txreqflitv, time, timev)
    logFlitRXRSP(id, vTime, clock, reset, rnId, rxrspflit, rxrspflitv, time, timev)
    logFlitRXDAT(id, vTime, clock, reset, rnId, rxdatflit, rxdatflitv, time, timev)
    logFlitRXSNP(id, vTime, clock, reset, rnId, rxsnpflit, rxsnpflitv, time, timev)
    logFlitTXRSP(id, vTime, clock, reset, rnId, txrspflit, txrspflitv, time, timev)
    logFlitTXDAT(id, vTime, clock, reset, rnId, txdatflit, txdatflitv, time, timev)
  }

  def logFlitsRNOfHNF(id          : String,
                      vTime     : Boolean,
                      clock       : Clock,
                      reset       : Reset,
                      hnId        : UInt,
                      rxreqflit   : UInt,
                      rxreqflitv  : Bool,
                      txrspflit   : UInt,
                      txrspflitv  : Bool,
                      txdatflit   : UInt,
                      txdatflitv  : Bool,
                      txsnpflit   : UInt,
                      txsnpflitv  : Bool,
                      rxrspflit   : UInt,
                      rxrspflitv  : Bool,
                      rxdatflit   : UInt,
                      rxdatflitv  : Bool,
                      time        : UInt = 0.U,
                      timev       : Bool = false.B): Unit = {
    logFlitRXREQ(id, vTime, clock, reset, hnId, rxreqflit, rxreqflitv, time, timev)
    logFlitTXRSP(id, vTime, clock, reset, hnId, txrspflit, txrspflitv, time, timev)
    logFlitTXDAT(id, vTime, clock, reset, hnId, txdatflit, txdatflitv, time, timev)
    logFlitTXSNP(id, vTime, clock, reset, hnId, txsnpflit, txsnpflitv, time, timev)
    logFlitRXRSP(id, vTime, clock, reset, hnId, rxrspflit, rxrspflitv, time, timev)
    logFlitRXDAT(id, vTime, clock, reset, hnId, rxdatflit, rxdatflitv, time, timev)
  }

  def logFlitsSNOfSNF(id          : String,
                      vTime     : Boolean,
                      clock       : Clock,
                      reset       : Reset,
                      snId        : UInt,
                      rxreqflit   : UInt,
                      rxreqflitv  : Bool,
                      txrspflit   : UInt,
                      txrspflitv  : Bool,
                      txdatflit   : UInt,
                      txdatflitv  : Bool,
                      rxdatflit   : UInt,
                      rxdatflitv  : Bool,
                      time        : UInt = 0.U,
                      timev       : Bool = false.B): Unit = {
    logFlitRXREQ(id, vTime, clock, reset, snId, rxreqflit, rxreqflitv, time, timev)
    logFlitTXRSP(id, vTime, clock, reset, snId, txrspflit, txrspflitv, time, timev)
    logFlitTXDAT(id, vTime, clock, reset, snId, txdatflit, txdatflitv, time, timev)
    logFlitRXDAT(id, vTime, clock, reset, snId, rxdatflit, rxdatflitv, time, timev)
  }

  def logFlitsSNOfHNF(id          : String,
                      vTime     : Boolean,
                      clock       : Clock,
                      reset       : Reset,
                      hnId        : UInt,
                      txreqflit   : UInt,
                      txreqflitv  : Bool,
                      rxrspflit   : UInt,
                      rxrspflitv  : Bool,
                      rxdatflit   : UInt,
                      rxdatflitv  : Bool,
                      txdatflit   : UInt,
                      txdatflitv  : Bool,
                      time        : UInt = 0.U,
                      timev       : Bool = false.B): Unit = {
    logFlitTXREQ(id, vTime, clock, reset, hnId, txreqflit, txreqflitv, time, timev)
    logFlitRXRSP(id, vTime, clock, reset, hnId, rxrspflit, rxrspflitv, time, timev)
    logFlitRXDAT(id, vTime, clock, reset, hnId, rxdatflit, rxdatflitv, time, timev)
    logFlitTXDAT(id, vTime, clock, reset, hnId, txdatflit, txdatflitv, time, timev)
  }
}
