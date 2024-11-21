/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2024 Institute of Computing Technology, Chinese Academy of Sciences
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
import chisel3.util.HasBlackBoxInline

class LogPerfIO extends Bundle {
  val timer = UInt(64.W)
  val logEnable = Bool()
  val clean = Bool()
  val dump = Bool()
}

class LogPerfHelper extends BlackBox with HasBlackBoxInline {
  val io = IO(Output(new LogPerfIO))

  val verilog =
    """`ifndef SIM_TOP_MODULE_NAME
      |  `define SIM_TOP_MODULE_NAME SimTop
      |`endif
      |
      |/*verilator tracing_off*/
      |
      |module LogPerfHelper (
      |  output [63:0] timer,
      |  output        logEnable,
      |  output        clean,
      |  output        dump
      |);
      |
      |assign timer         = `SIM_TOP_MODULE_NAME.logPerfSignal_timer;
      |assign logEnable     = `SIM_TOP_MODULE_NAME.logPerfSignal_logEnable;
      |assign clean         = `SIM_TOP_MODULE_NAME.logPerfSignal_clean;
      |assign dump          = `SIM_TOP_MODULE_NAME.logPerfSignal_dump;
      |
      |endmodule
      |
      |""".stripMargin
  setInline("LogPerfHelper.v", verilog)
}

object LogPerfHelper {
  private var logPerfSignal: Option[LogPerfIO] = None
  private var registered: Boolean = false

  def registerSignal(signal: LogPerfIO): Unit = {
    require(registered == false, "registerSignal can only be called once")
    registered = true
    logPerfSignal = Some(signal)
    signal.suggestName("logPerfSignal")
    dontTouch(signal)
  }

  def getSignal: LogPerfIO = chisel3.BuildInfo.version match {
    case s"3.$_.$_" => Module(new LogPerfHelper).io
    case _ => logPerfSignal.map(s => BoringUtils.tapAndRead(s)).getOrElse(0.U.asTypeOf(new LogPerfIO))
  }
}
