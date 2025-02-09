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
import chisel3.reflect.DataMirror.isVisible
import chisel3.util.experimental.BoringUtils.tapAndRead
import chisel3.experimental.BaseModule
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
  data: Seq[Data],
  moduleTag: String
)

class LogPerfIO extends Bundle {
  val timer = UInt(64.W)
  val logEnable = Bool()
  val clean = Bool()
  val dump = Bool()
}

object XSLog {
  private val logInfos = ListBuffer.empty[LogPerfParam]
  private val callBacks = ListBuffer.empty[(LogPerfIO) => Unit]
  private val logModules = ListBuffer.empty[BaseModule]

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
        data += WireInit(y.bits)
        y match {
          case Decimal(d) => fmt += "%d"
          case Hexadecimal(x) => fmt += "%x"
          case Binary(b) => fmt += "%b"
          case Character(c) => fmt += "%c"
        }
      }
      case PString(str) => fmt += str
      case other => throw new IllegalArgumentException("XSLog not support unpack: " + other.toString)
    }
    (fmt.mkString(""), data.toSeq)
  }

  // Access source Data in or not in current Module, called in LogPerfEndpoint to wrap boring wires
  def tapInfos: Seq[LogPerfParam] = {
    def tapOrGet[T <: Data](data: T): T = {
      if (isVisible(data)) data else tapAndRead(data)
    }

    logInfos.toSeq.map { info =>
      info.copy(
        cond = tapOrGet(info.cond),
        data = info.data.map(d => tapOrGet(d))
      )
    }
  }

  // XSPerf depends on passed LogPerfIO, so they deferred apply to endpoint
  // so original module should be passed for hierarchical module name
  def apply(debugLevel: XSLogLevel, curModOpt: Option[BaseModule])
           (prefix: Boolean, cond: Bool, pable: Printable)(implicit p: Parameters): Unit = {
    val logOpts = p(LogUtilsOptionsKey)
    val enableDebug = logOpts.enableDebug && debugLevel != XSLogLevel.PERF
    val enablePerf = logOpts.enablePerf && debugLevel == XSLogLevel.PERF
    if (!logOpts.fpgaPlatform && (enableDebug || enablePerf || debugLevel == XSLogLevel.ERROR)) {
      require(chisel3.XSCompatibility.currentWhen.isEmpty,
        "XSLog to be collect not supported inside whenContext, use XSLog(cond, pable) instead")
      require(chisel3.XSCompatibility.currentModule.isDefined, "XSLog should be called inside a module")
      val curMod = curModOpt.getOrElse(chisel3.XSCompatibility.currentModule.get)
      if (!logModules.contains(curMod)) logModules += curMod
      val (fmt, data) = unpackPrintable(pable)
      val moduleTag = curMod.toString
      // Deferred and buffered apply()
      if (debugLevel == XSLogLevel.ERROR) {
        when(cond) {
          assert(false.B) //assert at current module for better error location
        }
      }
      logInfos.addOne(LogPerfParam(debugLevel, prefix, cond, fmt, data, moduleTag))
    }
  }

  def apply(debugLevel: XSLogLevel)
           (prefix: Boolean, cond: Bool, pable: Printable)(implicit p: Parameters): Unit = {
    apply(debugLevel, None)(prefix, cond, pable)
  }

  def collect(ctrl: LogPerfIO)(implicit p: Parameters): Unit = {
    val logEndpoint = Module(new LogPerfEndpoint())
    logEndpoint.io := ctrl
  }

  def collect(timer: UInt, logEnable: Bool, clean: Bool, dump: Bool)(implicit p: Parameters): Unit = {
    val ctrl = Wire(new LogPerfIO)
    ctrl.timer := timer
    ctrl.logEnable := logEnable
    ctrl.clean := clean
    ctrl.dump := dump
    collect(ctrl)
  }

  // As XSPerf depends on LogPerfIO, their apply will be buffered until collection
  // Register collect() method from Callers when first apply, then call that during collection
  def registerCaller(caller: LogPerfIO => Unit): Unit = callBacks += caller
  def invokeCaller(ctrl: LogPerfIO): Unit = callBacks.foreach(caller => caller(ctrl))

  // Should only be called during firrtl phase(ChiselStage)
  // PathName can not be accessed until circuit elaboration
  def replaceFIRStr(str: String): String = {
    logModules.foldLeft(str) { case (acc, mod) =>
      acc.replace(mod.toString, mod.pathName)
    }
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

private class LogPerfEndpoint()(implicit p: Parameters) extends Module {
  val io = IO(Input(new LogPerfIO))
  def concatAndPrint(infos: Seq[LogPerfParam]): Unit = {
    infos.grouped(1000).foreach { infos =>
      val pable = infos.map { info =>
        val commonInfo = p"[${info.debugLevel}][time=${io.timer}] ${info.moduleTag}: "
        (if (info.prefix) commonInfo else p"") + Printable.pack(info.fmt, info.data: _*)
      }.reduce(_ + _)
      printf(pable)
    }
  }

  // To collect deferred call from XSPerf/..., invoke all registered caller
  XSLog.invokeCaller(io)
  // Group printfs with same cond to reduce system tasks for better thread schedule
  XSLog.tapInfos.groupBy(_.cond).values.foreach{ infos =>
    val cond = infos.head.cond
    val errs = infos.filter(_.debugLevel == XSLogLevel.ERROR)
    if (!errs.isEmpty) {
      when (cond) {
        concatAndPrint(errs)
        // assert at local module for better error location
      }
    }
    val logs = infos.filterNot(_.debugLevel == XSLogLevel.ERROR)
    if (!logs.isEmpty) {
      when(io.logEnable && cond) {
        concatAndPrint(logs)
      }
    }
  }
}
