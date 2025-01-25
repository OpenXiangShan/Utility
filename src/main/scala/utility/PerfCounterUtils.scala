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
import chisel3.util._
import chisel3.util.experimental.BoringUtils.tapAndRead
import chisel3.reflect.DataMirror.isVisible
import chisel3.experimental.BaseModule
import org.chipsalliance.cde.config.{Field, Parameters}

import scala.collection.mutable.ListBuffer
import XSPerfLevel.XSPerfLevel

case object PerfCounterOptionsKey extends Field[PerfCounterOptions]
case class PerfCounterOptions
(
  enablePerfPrint: Boolean,
  enablePerfDB: Boolean,
  perfLevel: XSPerfLevel,
  perfDBHartID: Int
)

object XSPerfLevel extends Enumeration {
  type XSPerfLevel = Value

  val VERBOSE  = Value(0, "VERBOSE")
  val NORMAL   = Value("NORMAL")
  val CRITICAL = Value("CRITICAL")
}

trait HasRegularPerfName {
  def judgeName(perfName: String): Unit = {
    val regular = """(\w+)""".r
    perfName match {
      case regular(_) => true
      case _ => {
        println("PerfName " + perfName + " is not '\\w+' regular")
        require(false)
      }
    }
  }
}

trait XSPerfTap {
  def tapOrGet[T <: Data](data: T): T ={
    if (isVisible(data)) data else tapAndRead(data)
  }
}

object XSPerfAccumulate extends HasRegularPerfName with XSPerfTap {
  private val perfInfos = ListBuffer.empty[(Option[BaseModule], String, UInt)]
  def apply(perfName: String, perfCnt: UInt, perfLevel: XSPerfLevel = XSPerfLevel.VERBOSE)
           (implicit p: Parameters): Unit = {
    judgeName(perfName)
    if (p(PerfCounterOptionsKey).enablePerfPrint && perfLevel >= p(PerfCounterOptionsKey).perfLevel) {
      if(perfInfos.isEmpty) XSLog.registerCaller(collect)
      perfInfos += ((chisel3.XSCompatibility.currentModule, perfName, perfCnt))
    }
  }
  def collect(ctrl: LogPerfIO)(implicit p: Parameters): Unit = {
    perfInfos.foreach{ case (curMod, perfName, perfCnt_bore) =>
      val perfCnt = tapOrGet(perfCnt_bore)
      val perfClean = ctrl.clean
      val perfDump = ctrl.dump
      val counter = RegInit(0.U(64.W)).suggestName(perfName + "Counter")
      val next_counter = WireInit(0.U(64.W)).suggestName(perfName + "Next")
      next_counter := counter + perfCnt
      counter := Mux(perfClean, 0.U, next_counter)

      XSPerfPrint(curMod)(perfDump, p"$perfName, $next_counter\n")
    }
  }
}

object XSPerfHistogram extends HasRegularPerfName with XSPerfTap {
  // instead of simply accumulating counters
  // this function draws a histogram
  private val perfHistInfos = ListBuffer.empty[(Option[BaseModule], String, UInt, Bool, Int, Int, Int, Boolean, Boolean)]
  def apply
  (
    perfName: String,
    perfCnt: UInt,
    enable: Bool,
    start: Int,
    stop: Int,
    step: Int = 1,
    left_strict: Boolean = false,
    right_strict: Boolean = false,
    perfLevel: XSPerfLevel = XSPerfLevel.VERBOSE
  )
  (implicit p: Parameters): Unit = {
    judgeName(perfName)
    if (p(PerfCounterOptionsKey).enablePerfPrint && perfLevel >= p(PerfCounterOptionsKey).perfLevel) {
      if(perfHistInfos.isEmpty) XSLog.registerCaller(collect)
      perfHistInfos += ((chisel3.XSCompatibility.currentModule, perfName, perfCnt, enable, start, stop, step, left_strict, right_strict))
    }
  }
  def collect(ctrl: LogPerfIO)(implicit p: Parameters): Unit = {
    perfHistInfos.foreach{ case (curMod, perfName, perfCnt_bore, enable_bore, start, stop, step, left_strict, right_strict) =>
      val perfCnt = tapOrGet(perfCnt_bore)
      val enable = tapOrGet(enable_bore)
      val perfClean = ctrl.clean
      val perfDump = ctrl.dump

      val sum = RegInit(0.U(64.W)).suggestName(perfName + "Sum")
      val nSamples = RegInit(0.U(64.W)).suggestName(perfName + "NSamples")
      val underflow = RegInit(0.U(64.W)).suggestName(perfName + "Underflow")
      val overflow = RegInit(0.U(64.W)).suggestName(perfName + "Overflow")
      when (perfClean) {
        sum := 0.U
        nSamples := 0.U
        underflow := 0.U
        overflow := 0.U
      } .elsewhen (enable) {
        sum := sum + perfCnt
        nSamples := nSamples + 1.U
        when (perfCnt < start.U) {
          underflow := underflow + 1.U
        }
        when (perfCnt >= stop.U) {
          overflow := overflow + 1.U
        }
      }

      XSPerfPrint(curMod)(perfDump, p"${perfName}_sum, ${sum}\n")
      XSPerfPrint(curMod)(perfDump, p"${perfName}_mean, ${sum/nSamples}\n")
      XSPerfPrint(curMod)(perfDump, p"${perfName}_sampled, ${nSamples}\n")
      XSPerfPrint(curMod)(perfDump, p"${perfName}_underflow, ${underflow}\n")
      XSPerfPrint(curMod)(perfDump, p"${perfName}_overflow, ${overflow}\n")

      // drop each perfCnt value into a bin
      val nBins = (stop - start) / step
      require(start >= 0)
      require(stop > start)
      require(nBins > 0)

      (0 until nBins) map { i =>
        val binRangeStart = start + i * step
        val binRangeStop = start + (i + 1) * step
        val inRange = perfCnt >= binRangeStart.U && perfCnt < binRangeStop.U

        // if perfCnt < start, it will go to the first bin
        val leftOutOfRange = if(left_strict)
          false.B
        else
          perfCnt < start.U && i.U === 0.U
        // if perfCnt >= stop, it will go to the last bin
        val rightOutOfRange = if(right_strict)
          false.B
        else
          perfCnt >= stop.U && i.U === (nBins - 1).U
        val inc = inRange || leftOutOfRange || rightOutOfRange

        val histName = s"${perfName}_${binRangeStart}_${binRangeStop}"
        val counter = RegInit(0.U(64.W)).suggestName(histName)
        when (perfClean) {
          counter := 0.U
        } .elsewhen(enable && inc) {
          counter := counter + 1.U
        }

        XSPerfPrint(curMod)(perfDump, p"${histName}, $counter\n")
      }
    }
  }
}

object XSPerfMax extends HasRegularPerfName with XSPerfTap {
  private val perfMaxInfos = ListBuffer.empty[(Option[BaseModule], String, UInt, Bool)]
  def apply(perfName: String, perfCnt: UInt, enable: Bool, perfLevel: XSPerfLevel = XSPerfLevel.VERBOSE)
           (implicit p: Parameters): Unit = {
    judgeName(perfName)
    if (p(PerfCounterOptionsKey).enablePerfPrint && perfLevel >= p(PerfCounterOptionsKey).perfLevel) {
      if(perfMaxInfos.isEmpty) XSLog.registerCaller(collect)
      perfMaxInfos += ((chisel3.XSCompatibility.currentModule, perfName, perfCnt, enable))
    }
  }

  def collect(ctrl: LogPerfIO)(implicit p: Parameters): Unit = {
    perfMaxInfos.foreach{ case (curMod, perfName, perfCnt_bore, enable_bore) =>
      val perfCnt = tapOrGet(perfCnt_bore)
      val enable = tapOrGet(enable_bore)
      val perfClean = ctrl.clean
      val perfDump = ctrl.dump

      val max = RegInit(0.U(64.W))
      val next_max = Mux(enable && (perfCnt > max), perfCnt, max)
      max := Mux(perfClean, 0.U, next_max)

      XSPerfPrint(curMod)(perfDump, p"${perfName}_max, $next_max\n")
    }
  }
}

object QueuePerf {
  def apply(size: Int, utilization: UInt, full: UInt, perfLevel: XSPerfLevel = XSPerfLevel.VERBOSE)
           (implicit p: Parameters): Unit = {
    XSPerfAccumulate("utilization", utilization, perfLevel)
    XSPerfHistogram("util", utilization, true.B, 0, size, 1, perfLevel = perfLevel)
    XSPerfAccumulate("full", full, perfLevel)
    val exHalf = utilization > (size/2).U
    val empty = utilization === 0.U
    XSPerfAccumulate("exHalf", exHalf,perfLevel)
    XSPerfAccumulate("empty", empty, perfLevel)
  }
}

object TransactionLatencyCounter {
  // count the latency between start signal and stop signal
  // whenever stop signals comes, we create a latency sample
  def apply(start: Bool, stop: Bool): (Bool, UInt) = {
    assert (!(start && stop))
    val counter = RegInit(0.U(64.W))
    val next_counter = counter + 1.U
    counter := Mux(start || stop, 0.U, next_counter)
    (stop, next_counter)
  }
}

object XSPerfRolling extends HasRegularPerfName {

  class RollingEntry()(implicit p: Parameters) extends Bundle {
    val xAxisPt = UInt(64.W)
    val yAxisPt = UInt(64.W)

    def apply(xAxisPt: UInt, yAxisPt: UInt): RollingEntry = {
      val e = Wire(new RollingEntry())
      e.xAxisPt := xAxisPt
      e.yAxisPt := yAxisPt
      e
    }
  }

  def apply(
    perfName: String,
    perfCnt: UInt,
    granularity: Int,
    clock: Clock,
    reset: Reset
  )(implicit p: Parameters): Unit = {
    judgeName(perfName)
    if (p(PerfCounterOptionsKey).enablePerfDB) {
      val tableName = perfName + "_rolling_" + p(PerfCounterOptionsKey).perfDBHartID.toString
      val rollingTable = ChiselDB.createTable(tableName, new RollingEntry(), basicDB=true)

      val xAxisCnt = RegInit(0.U(64.W))
      val yAxisCnt = RegInit(0.U(64.W))
      val xAxisPtReg = RegInit(0.U(64.W))
      val xAxisPt = WireInit(0.U(64.W))
      xAxisCnt := xAxisCnt + 1.U(64.W)  // increment per cycle
      yAxisCnt := yAxisCnt + perfCnt

      val triggerDB = xAxisCnt === granularity.U
      when(triggerDB) {
        xAxisCnt := 1.U(64.W)
        yAxisCnt := perfCnt
        xAxisPtReg := xAxisPtReg + granularity.U
        xAxisPt := xAxisPtReg + granularity.U
      }
      val rollingPt = new RollingEntry().apply(xAxisPt, yAxisCnt)
      rollingTable.log(rollingPt, triggerDB, "", clock, reset)
    }
  }

  def apply(
    perfName: String,
    perfCnt: UInt,
    eventTrigger: UInt,
    granularity: Int,
    clock: Clock,
    reset: Reset
  )(implicit p: Parameters): Unit = {
    judgeName(perfName)
    if (p(PerfCounterOptionsKey).enablePerfDB) {
      val tableName = perfName + "_rolling_" + p(PerfCounterOptionsKey).perfDBHartID.toString
      val rollingTable = ChiselDB.createTable(tableName, new RollingEntry(), basicDB=true)

      val xAxisCnt = RegInit(0.U(64.W))
      val yAxisCnt = RegInit(0.U(64.W))
      val xAxisPtReg = RegInit(0.U(64.W))
      val xAxisPt = WireInit(0.U(64.W))
      xAxisCnt := xAxisCnt + eventTrigger // increment when event triggers
      yAxisCnt := yAxisCnt + perfCnt

      val triggerDB = xAxisCnt >= granularity.U
      when(triggerDB) {
        xAxisCnt := xAxisCnt - granularity.U + eventTrigger
        yAxisCnt := perfCnt
        xAxisPtReg := xAxisPtReg + xAxisCnt
        xAxisPt := xAxisPtReg + xAxisCnt
      }
      val rollingPt = new RollingEntry().apply(xAxisPt, yAxisCnt)
      rollingTable.log(rollingPt, triggerDB, "", clock, reset)
    }
  }

  // event interval based mode
  def apply(
    perfName: String,
    perfCntX: UInt,
    perfCntY: UInt,
    granularity: Int,
    eventTrigger: UInt,
    clock: Clock,
    reset: Reset
  )(implicit p: Parameters): Unit = {
    judgeName(perfName)
    if (p(PerfCounterOptionsKey).enablePerfDB) {
      val tableName = perfName + "_rolling_" + p(PerfCounterOptionsKey).perfDBHartID.toString
      val rollingTable = ChiselDB.createTable(tableName, new RollingEntry(), basicDB=true)

      val xAxisCnt = RegInit(0.U(64.W))
      val yAxisCnt = RegInit(0.U(64.W))
      val eventCnt = RegInit(0.U(64.W))
      xAxisCnt := xAxisCnt + perfCntX
      yAxisCnt := yAxisCnt + perfCntY
      eventCnt := eventCnt + eventTrigger

      val triggerDB = eventCnt >= granularity.U
      when(triggerDB) {
        eventCnt := eventTrigger
        xAxisCnt := perfCntX
        yAxisCnt := perfCntY
      }
      val rollingPt = new RollingEntry().apply(xAxisCnt, yAxisCnt)
      rollingTable.log(rollingPt, triggerDB, "", clock, reset)
    }
  }
}

object XSPerfPrint {
  // XSPerf depends on LogPerfIO passed from Top, defer apply and collect to endpoint
  // XSLog is also deferred apply in another module, pass original module for pathName
  def apply(modOpt: Option[BaseModule])(cond: Bool, pable: Printable)(implicit p: Parameters): Unit = {
    XSLog(XSLogLevel.PERF, modOpt)(true, cond, pable)
  }
}
