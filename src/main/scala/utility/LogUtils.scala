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
import org.chipsalliance.cde.config.{Field, Parameters}
import XSLogLevel.XSLogLevel

import scala.collection.mutable.ListBuffer

case object LogUtilsOptionsKey extends Field[LogUtilsOptions]
case class LogUtilsOptions
(
  enableDebug: Boolean,
  enablePerf: Boolean,
  fpgaPlatform: Boolean
)

object XSLogLevel extends Enumeration {
  type XSLogLevel = Value

  val ALL   = Value(0, "ALL  ")
  val DEBUG = Value("DEBUG")
  val INFO  = Value("INFO ")
  val PERF  = Value("PERF ")
  val WARN  = Value("WARN ")
  val ERROR = Value("ERROR")
  val OFF   = Value("OFF  ")
}

// Correspond to XSLog apply
case class LogPerfParam (
  debugLevel: XSLogLevel,
  prefix: Boolean,
  cond: Bool,
  fmt: String,
  data: Seq[Data]
)

object XSLog {
  val MagicStr = "__PERCENTAGE_M__"
  private val logInfos = ListBuffer.empty[LogPerfParam]
  private val callBacks = ListBuffer.empty[LogPerfIO => Unit]

  private def mark[T <: Data](data: T): T = {
    val bore = WireInit(data)
    dontTouch(bore)
    bore
  }

  private def unpackPrintable(pable: Printable): (String, Seq[Data]) = {
    val fmt = ListBuffer.empty[String]
    val data = ListBuffer.empty[Data]
    pable match {
      case Printables(p) => p.foreach { pb =>
        val (subfmt, subdata) = unpackPrintable(pb)
        fmt += subfmt
        data ++= subdata
      }
      case y: FirrtlFormat => {
        data += mark(y.bits)
        y match {
          case Decimal(d) => fmt += "%d"
          case Hexadecimal(x) => fmt += "%x"
          case Binary(b) => fmt += "%b"
          case Character(c) => fmt += "%c"
        }
      }
      case PString(str) => {
        fmt += str.replace("\n", "\\n")
      }
      case other => throw new IllegalArgumentException("XSLog not support unpack: " + other.toString)
    }
    (fmt.mkString(""), data.toSeq)
  }

  def apply(debugLevel: XSLogLevel, ctrlInfoOpt: Option[LogPerfIO])
           (prefix: Boolean, cond: Bool, pable: Printable)(implicit p: Parameters): Unit = {
    val logOpts = p(LogUtilsOptionsKey)
    val enableDebug = logOpts.enableDebug && debugLevel != XSLogLevel.PERF
    val enablePerf = logOpts.enablePerf && debugLevel == XSLogLevel.PERF
    if (!logOpts.fpgaPlatform && (enableDebug || enablePerf || debugLevel == XSLogLevel.ERROR)) {
      if (ctrlInfoOpt.isDefined) { // keep original code, while using endpoint by default
        val ctrlInfo = ctrlInfoOpt.getOrElse(Module(new LogPerfHelper).io)
        val logEnable = ctrlInfo.logEnable
        val logTimestamp = ctrlInfo.timer
        val check_cond = (if (debugLevel == XSLogLevel.ERROR) true.B else logEnable) && cond
        when (check_cond) {
          val commonInfo = p"[$debugLevel][time=$logTimestamp] $MagicStr: "
          printf((if (prefix) commonInfo else p"") + pable)
          if (debugLevel >= XSLogLevel.ERROR) {
            assert(false.B)
          }
        }
      } else {
        val (fmt, data) = unpackPrintable(pable)
        logInfos.addOne(LogPerfParam(debugLevel, prefix, mark(cond), fmt, data))
      }
    }
  }

  def apply(debugLevel: XSLogLevel)
           (prefix: Boolean, cond: Bool, pable: Printable)(implicit p: Parameters): Unit = {
    apply(debugLevel, None)(prefix, cond, pable)
  }

  def apply(debugLevel: XSLogLevel, ctrlInfoOpt: LogPerfIO)
           (prefix: Boolean, cond: Bool, pable: Printable)(implicit p: Parameters): Unit = {
    apply(debugLevel, Some(ctrlInfoOpt))(prefix, cond, pable)
  }

  def registerCaller(caller: LogPerfIO => Unit): Unit = callBacks += caller

  def collect(ctrl: LogPerfIO)(implicit p: Parameters): Unit = {
    val endpoint = Module(new LogPerfEndpoint(logInfos.toSeq))
    endpoint.io := ctrl
//    callBacks.foreach{ caller =>
//      caller(ctrl)
//    }
  }

  def collect(timer: UInt, logEnable: Bool, clean: Bool, dump: Bool)(implicit p: Parameters): Unit = {
    val ctrl = Wire(new LogPerfIO)
    ctrl.timer := timer
    ctrl.logEnable := logEnable
    ctrl.clean := clean
    ctrl.dump := dump
    collect(ctrl)
  }

  // Only called after circuit elaboration
  def emitVerilog(): Unit = {
    def getPath(data: Data): String = data.pathName.replace("inner.", "inner_")
    def getModPath(data: Data): String = data.parentPathName.replace("inner.", "inner_")
    val logPrints = logInfos.map { info =>
      val (pfxFmt, pfxArg): (String, Seq[String]) = if (info.prefix) {
        // Mark cond to module before, so we can use cond to get module path
        (s"[${info.debugLevel}][time=%d] ${getModPath(info.cond)}: ", Seq("io_timer"))
      } else {
        ("", Seq())
      }
      val cond = getPath(info.cond) + (if (info.debugLevel == XSLogLevel.ERROR) "" else " && io_logEnable")
      val fmt = pfxFmt + info.fmt
      val argStr = (pfxArg ++ info.data.map(d => getPath(d))).mkString(", ")
      s"if (${cond}) $$fwrite(32'h80000002, \"${fmt}\", ${argStr});"
    }
    val verilog =
      s"""
         |module LogPerfPrinter(
         |  input clock,
         |  input [63:0] io_timer,
         |  input io_logEnable,
         |  input io_clean,
         |  input io_dump
         |);
         |always @(posedge clock) begin
         |  ${logPrints.mkString("\n  ")}
         |end
         |endmodule
         |""".stripMargin
    FileRegisters.writeOutputFile("./build/rtl", "LogPerfPrinter.v", verilog)
  }

}

sealed abstract class LogHelper(val logLevel: XSLogLevel){

  def apply(cond: Bool, fmt: String, data: Bits*)(implicit p: Parameters): Unit =
    apply(cond, Printable.pack(fmt, data:_*))
  def apply(cond: Bool, pable: Printable)(implicit p: Parameters): Unit = apply(true, cond, pable)
  def apply(fmt: String, data: Bits*)(implicit p: Parameters): Unit =
    apply(Printable.pack(fmt, data:_*))
  def apply(pable: Printable)(implicit p: Parameters): Unit = apply(true.B, pable)
  def apply(prefix: Boolean, cond: Bool, fmt: String, data: Bits*)(implicit p: Parameters): Unit =
    apply(prefix, cond, Printable.pack(fmt, data:_*))
  def apply(prefix: Boolean, cond: Bool, pable: Printable)(implicit p: Parameters): Unit ={
    XSLog(logLevel)(prefix, cond, pable)
  }
}

object XSDebug extends LogHelper(XSLogLevel.DEBUG)

object XSInfo extends LogHelper(XSLogLevel.INFO)

object XSWarn extends LogHelper(XSLogLevel.WARN)

object XSError extends LogHelper(XSLogLevel.ERROR)
