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

package utility

import chisel3._
import chisel3.util._
import utility.CLogBWriteRecord._
import utility.CLogBWriteParameters._

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
    var singleton = true
}

private class CLogBWriteParameters(id: String, params: CHIParameters) extends BlackBox with HasBlackBoxInline {

    val io = IO(new Bundle {
        val clock = Input(Clock())
        val reset = Input(Reset())
        val en = Input(Bool())
    })

    require(singleton, "singleton")
    singleton = false

    val moduleName = "CLogB_StubWriteParameters"
    val dpicFunc = "CLogB_SharedWriteParameters"

    val verilog =
        s"""
        |import "DPI-C" function void $dpicFunc (
        |    input   string              id,
        |    input   int                 issue,
        |    input   int                 nodeIdWidth,
        |    input   int                 addrWidth,
        |    input   int                 reqRsvdcWidth,
        |    input   int                 datRsvdcWidth,
        |    input   int                 dataWidth,
        |    input   int                 dataCheckPresent,
        |    input   int                 poisonPresent,
        |    input   int                 mpamPresent
        |);
        |
        |module $moduleName (
        |    input   logic               clock,
        |    input   logic               reset,
        |    input   logic               en
        |);
        |
        |    reg singleton;
        |
        |    always @(posedge clock) begin
        |
        |        if (reset) begin
        |            singleton <= 1'b0;
        |        end
        |        else if (en && !reset && !singleton) begin
        |            singleton <= 1'b1;
        |            $dpicFunc(
        |                \"${id}\", 
        |                ${params.issue}, /* issue */
        |                ${params.nodeIdWidth}, /* nodeIdWidth */
        |                ${params.reqAddrWidth}, /* addrWidth */
        |                ${params.reqRsvdcWidth}, /* reqRsvdcWidth */
        |                ${params.datRsvdcWidth}, /* datRsvdcWidth */
        |                ${params.dataWidth}, /* dataWidth */
        |                ${if (params.dataCheckPresent) "1" else "0"}, /* dataCheckPresent */
        |                ${if (params.poisonPresent) "1" else "0"}, /* poisonPresent */
        |                ${if (params.mpamPresent) "1" else "0"} /* mpamPresent */
        |            );
        |        end
        |    end
        |
        |endmodule
        """.stripMargin

    setInline(s"$moduleName.sv", verilog)

    override def desiredName: String = moduleName
}
//

/*
* CLog.B topologies write operations.
*/
private object CLogBWriteTopo {
    var uniqueCounter = -1
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

    val suffixName = if (uniqueName.isEmpty) s"${nextUniqueIndex}" else uniqueName

    val moduleName = s"CLogB_StubWriteTopo_${suffixName}_${desc}"
    val dpicFuncWrite = "CLogB_SharedWriteTopo"
    val dpicFuncEnd = "CLogB_SharedWriteTopoEnd"

    val verilog =
        s"""
        |import "DPI-C" function void $dpicFuncWrite (
        |    input   string              id,
        |    input   int                 nodeId,
        |    input   int                 nodeType
        |);
        |
        |import "DPI-C" function void $dpicFuncEnd (
        |    input   string              id
        |);
        |
        |/* $desc */
        |module $moduleName (
        |    input   logic               clock,
        |    input   logic               reset,
        |    input   logic               en
        |);
        |
        |    always @(posedge clock) begin
        |
        |        if (en && !reset) begin
        |${topo.map{ case (nid, typ) => s"            $dpicFuncWrite(\"${id}\", $nid, $typ);" }.mkString("\n")}
        |            $dpicFuncEnd(\"${id}\");
        |        end
        |    end
        |
        |endmodule
        """.stripMargin
    
    setInline(s"$moduleName.sv", verilog)

    override def desiredName: String = moduleName
}
//

/* 
* CLog.B log record write operations. 
*/
private object CLogBWriteRecord {
    var uniqueCounter = -1
    def nextUniqueIndex: Int = {
        uniqueCounter = uniqueCounter + 1
        uniqueCounter
    }
}

private class CLogBWriteRecord(id: String, channel: Int, flitLength: Int, uniqueName: String = "", desc: String = "") extends BlackBox with HasBlackBoxInline {

    val io = IO(new Bundle {
        val clock = Input(Clock())
        val reset = Input(Reset())
        val en = Input(Bool())
        val cycleTime = Input(UInt(64.W))
        val nodeId = Input(UInt(32.W))
        val flit = Input(UInt(512.W))
    })

    val suffixName = if (uniqueName.isEmpty) s"${nextUniqueIndex}" else uniqueName

    val moduleName = s"CLogB_StubWriteRecord_${suffixName}_${desc}"
    val dpicFunc = "CLogB_SharedWriteRecord"

    val verilog =
        s"""
        |import "DPI-C" function void $dpicFunc (
        |    input   string              id,
        |    input   longint             cycletime,
        |    input   int                 nodeId,
        |    input   int                 channel,
        |    input   bit [511:0]         flit,
        |    input   int                 flitLength
        |);
        |
        |/* $desc */
        |module $moduleName (
        |    input   logic               clock,
        |    input   logic               reset,
        |    input   logic               en,
        |    input   longint             cycleTime,
        |    input   int                 nodeId,
        |    input   bit [511:0]         flit
        |);
        |
        |    always @(posedge clock) begin
        |
        |        if (en && !reset) begin
        |            $dpicFunc(\"${id}\", cycleTime, nodeId, 32'd${channel}, flit, 32'd${flitLength});
        |        end
        |    end
        |
        |endmodule
        """.stripMargin

    setInline(s"$moduleName.sv", verilog)

    override def desiredName: String = moduleName
}
//

object CHIron {

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

    def logFlit(desc: String, id: String, channel: Int, clock: Clock, reset: Reset, nodeId: UInt, flit: UInt, flitv: Bool): Unit = {
        if (enable) {
            val stub = Module(new CLogBWriteRecord(id, channel, flit.getWidth, "", desc))
            val cycle = RegInit(0.U(64.W))
            cycle := cycle + 1.U
            stub.io.clock := clock
            stub.io.reset := reset
            stub.io.en := flitv
            stub.io.cycleTime := cycle
            stub.io.nodeId := nodeId
            stub.io.flit := flit
        }
    }

    def logFlitTXREQ(id: String, clock: Clock, reset: Reset, nodeId: UInt, flit: UInt, flitv: Bool): Unit = {
        logFlit("TXREQ", id, CLog.ChannelTXREQ, clock, reset, nodeId, flit, flitv)
    }

    def logFlitTXRSP(id: String, clock: Clock, reset: Reset, nodeId: UInt, flit: UInt, flitv: Bool): Unit = {
        logFlit("TXRSP", id, CLog.ChannelTXRSP, clock, reset, nodeId, flit, flitv)
    }

    def logFlitTXDAT(id: String, clock: Clock, reset: Reset, nodeId: UInt, flit: UInt, flitv: Bool): Unit = {
        logFlit("TXDAT", id, CLog.ChannelTXDAT, clock, reset, nodeId, flit, flitv)
    }

    def logFlitTXSNP(id: String, clock: Clock, reset: Reset, nodeId: UInt, flit: UInt, flitv: Bool): Unit = {
        logFlit("TXSNP", id, CLog.ChannelTXSNP, clock, reset, nodeId, flit, flitv)
    }

    def logFlitRXREQ(id: String, clock: Clock, reset: Reset, nodeId: UInt, flit: UInt, flitv: Bool): Unit = {
        logFlit("RXREQ", id, CLog.ChannelRXREQ, clock, reset, nodeId, flit, flitv)
    }

    def logFlitRXRSP(id: String, clock: Clock, reset: Reset, nodeId: UInt, flit: UInt, flitv: Bool): Unit = {
        logFlit("RXRSP", id, CLog.ChannelRXRSP, clock, reset, nodeId, flit, flitv)
    }

    def logFlitRXDAT(id: String, clock: Clock, reset: Reset, nodeId: UInt, flit: UInt, flitv: Bool): Unit = {
        logFlit("RXDAT", id, CLog.ChannelRXDAT, clock, reset, nodeId, flit, flitv)
    }

    def logFlitRXSNP(id: String, clock: Clock, reset: Reset, nodeId: UInt, flit: UInt, flitv: Bool): Unit = {
        logFlit("RXSNP", id, CLog.ChannelRXSNP, clock, reset, nodeId, flit, flitv)
    }
}
