/***************************************************************************************
* Copyright (c) 2024-2026 Beijing Institute of Open Source Chip
*
* Utility is licensed under Mulan PSL v2.
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

package utility.Xstatistics

import chisel3._
import chisel3.util.HasBlackBoxInline
import scala.collection.mutable.ArrayBuffer
import utility.HasDPICUtils
import freechips.rocketchip.diplomacy.BufferParams.default
import utility.FileRegisters
import scala.collection.mutable.ListBuffer
import utility.XSLog
import utility.LogPerfIO
import org.chipsalliance.cde.config.Parameters
import scala.util

trait NodeExpr {
  def +(b: NodeExpr) = {
    ConstExpr("(" + exprStr() + "+" + b.exprStr() + ")")
  }
  def -(b: NodeExpr) = {
    ConstExpr("(" + exprStr() + "-" + b.exprStr() + ")")
  }
  def *(b: NodeExpr) = {
    ConstExpr("(" + exprStr() + "*" + b.exprStr() + ")")
  }
  def /(b: NodeExpr) = {
    ConstExpr("(" + "(double)" + exprStr() + "/" + b.exprStr() + ")")
  }

  def funcWrapping(mathFunc: String, argmap: String = "x") {
    val arg = argmap.replace("x", exprStr())
    ConstExpr(mathFunc + s"(${arg})")
  }

  // virtual:
  def exprStr() = this match {
    case node: Node => node.varname()
    case _ => ???
  }
}

class ConstExpr(private val express: String) extends NodeExpr {
  override def exprStr() = express
}

abstract class Node(val origin_name: String, val descript: String) {
  judgeName(origin_name)
  private val curModule = chisel3.XSCompatibility.currentModule.getOrElse(chisel3.XSCompatibility.currentModule.get)
  private val full_name = "Xstat" + curModule.hashCode.toHexString + "__" + origin_name
  protected val dpic_func = s"dpic_${varname()}"
  XstatsMgr.register(this)

  private def judgeName(perfName: String): Unit = {
    val regular = """(\w+)""".r
    perfName match {
      case regular(_) =>
      case _ => {
        throw new Exception("Statistics:  " + perfName + " is not '\\w+' regular")
      }
    }
  }

  def varname() = full_name
  // modulePath only can be called after chisel elaborate
  private def modulePath() = curModule.pathName
  protected def dumpFormat(name: String, v: String) ="\"" + modulePath + "." + name + "\\t\\t\\t\" << " + v + s" << \"\\t\\t\\t#${descript}\\n\""

  // virtual:
  def getVarDef() = s"uint64_t ${full_name} = 0;"
  // return the cpp code of dump and reset
  def getDumpingAndReset(): (String, String) = {
    (s"dout << ${dumpFormat(origin_name, varname())};", s"${varname()} = 0;")
  }
  def getDPIC(): String = ""
}

class Scalar(name: String, desc: String) extends Node(name, desc) with NodeExpr {
  private class ScalarModule(name: String) extends HasDPICUtils {
    val io = IO(new Bundle{
      val clock = Input(Clock())
      val reset = Input(Reset())
      val en = Input(Bool())
      val n = Input(UInt(64.W))
    })
    init(io, false, false, name)
  }

  def sample(n: UInt, clock: Clock) {
    val m = Module(new ScalarModule(dpic_func))
    m.io.clock := clock
    m.io.reset := false.B
    m.io.en := true.B
    m.io.n := n
  }

  def sample(n: UInt, en: Bool, clock: Clock) {
    val m = Module(new ScalarModule(dpic_func))
    m.io.clock := clock
    m.io.reset := false.B
    m.io.en := en
    m.io.n := n
  }

  override def getDPIC() = {
    s"""
    |void ${dpic_func}(uint64_t val) {
    |  ${varname()} += val;
    |}
    """.stripMargin
  }
}

class Vector(n: Int, name: String, desc: String) extends Node(name, desc) {
  val size = n
  protected var subnames = new Array[String](n)

  subnames.zipWithIndex.foreach{ case (s, i) =>
    subnames(i) = i.toString()
  }

  def apply(i : Int) = {
    if (i >= size) {
      throw new Exception(s"Statistics: ${varname()} out of range")
    }
    ConstExpr(varname() + s"[${i}]")
  }

  def sum() = {
    ConstExpr(s"std::accumulate(${varname()}.begin(), ${varname()}.end(), 0)")
  }
  
  def maxCount() = {
    ConstExpr(s"*std::max_element(${varname()}.begin(), ${varname()}.end());")
  }

  def minCount() = {
    ConstExpr(s"*std::min_element(${varname()}.begin(), ${varname()}.end());")
  }

  def setSubname(name: Array[String]) {
    require(subnames.size == name.size)
    subnames = name
  }

  private class VectorModule(name: String) extends HasDPICUtils {
    val io = IO(new Bundle{
      val clock = Input(Clock())
      val reset = Input(Reset())
      val en = Input(Bool())
      val i = Input(UInt(64.W))
      val n = Input(UInt(64.W))
    })
    init(io, false, false, name)
  }

  /**
    * counter[i] += n
    *  
    * @param i the input value ( < size or <= max)
    * @param n the count of value
    * @param en sample if enable
    */
  def sample(i: UInt, n: UInt, en: Bool, clock: Clock) = {
    val m = Module(new VectorModule(dpic_func))
    m.io.clock := clock
    m.io.reset := false.B
    m.io.en := en
    m.io.i := i
    m.io.n := n
  }

  /**
    * counter[i]++
    * 
    * @param i the input value ( < size or <= max)
    */
  def sample(i: UInt, clock: Clock) = {
    val m = Module(new VectorModule(dpic_func))
    m.io.clock := clock
    m.io.reset := false.B
    m.io.en := true.B
    m.io.i := i
    m.io.n := 1.U
  }

  override def getVarDef() = s"std::array<uint64_t, ${size}> ${varname()};"

  override def getDumpingAndReset() = {
    val dumpstr = s"${
      (for (i <- 0 until size) yield (origin_name + "::" + subnames(i), varname() + s"[${i}]")).map{case (n, v) =>
        s"dout << ${dumpFormat(n, v)};\n"
      }.reduce(_ + _)
    }"
    val resetstr = s"std::fill(${varname()}.begin(), ${varname()}.end(), 0);"
    (dumpstr, resetstr)
  }

  override def getDPIC() = {
    s"""
    |void ${dpic_func}(uint64_t index, uint64_t val) {
    |  if (index < ${size}) {
    |    ${varname()}[index] += val;
    |  }
    |}
    """.stripMargin
  }
}

class Distribution(val min: Int, val max: Int, val bkt: Int, name: String, desc: String) extends Vector(((max + 1 - min).toFloat / bkt).ceil.round, name, desc) {
  // couner <- [min, max], group by bkt
  private val total = varname() + "_total"
  private val overflows = varname() + "_overflows"
  private val minvalue = varname() + "_min_value"
  private val maxvalue = varname() + "_max_value"

  require(bkt > 0)
  require((max - min) / bkt < size)
  for (i <- 0 until size) {
    // [low, up]
    val low = i * bkt + min
    var up = (i + 1) * bkt + min - 1
    up = math.min(up, max)
    subnames(i) = s"${low}-${up}"
  }

  override def sum() = ConstExpr(total)
  def mean() = ConstExpr(s"((double)${total} / (${super.sum().exprStr()} + ${overflows}))")
  def minValue() = ConstExpr(minvalue)
  def maxValue() = ConstExpr(maxvalue)

  override def getVarDef() = {
    val s = super.getVarDef()
    s + s"""
    |uint64_t ${total} = 0;
    |uint64_t ${overflows} = 0;
    |uint64_t ${minvalue} = 0;
    |uint64_t ${maxvalue} = 0;
    """.stripMargin
  }
  override def getDumpingAndReset() = {
    var (dumpstr, resetstr) = super.getDumpingAndReset()
    dumpstr += s"""
    |dout << ${dumpFormat(origin_name + "::" + "total", total)};
    |dout << ${dumpFormat(origin_name + "::" + "mean", mean().exprStr())};
    |dout << ${dumpFormat(origin_name + "::" + "overflows", overflows)};
    |dout << ${dumpFormat(origin_name + "::" + "min_value", minvalue)};
    |dout << ${dumpFormat(origin_name + "::" + "max_value", maxvalue)};
    """.stripMargin
    resetstr += s"""
    |${total} = 0;
    |${overflows} = 0;
    |${minvalue} = 0;
    |${maxvalue} = 0;
    """.stripMargin
    (dumpstr, resetstr)
  }
  override def getDPIC() = {
    s"""
    |void ${dpic_func}(uint64_t index, uint64_t val) {
    |  if (index <= ${max} && index >= ${min}) {
    |    unsigned t = (unsigned)(std::floor(index - ${min}) / ${bkt});
    |    ${varname()}[t] += val;
    |  } else {
    |    ${overflows} += val;
    |  }
    |  ${total} += index * val;
    |  ${minvalue} = std::min(index, ${minvalue});
    |  ${maxvalue} = std::max(index, ${maxvalue});
    |}
    """.stripMargin
  }
}

class Formula(name: String, desc: String) extends Node(name, desc) with NodeExpr {
  var express: String = ""
  def :=(expr: NodeExpr) {
    express = expr.exprStr()
  }

  override def getVarDef() = ""
  override def getDumpingAndReset() = {
    (s"dout << ${dumpFormat(origin_name, exprStr())};", "")
  }
  override def exprStr() = express
}

// anonymous counter, no need to dump
class AnoScalar[T <: Node] extends Scalar(util.Random.alphanumeric.take(8).mkString, "") {
  override def getDumpingAndReset() = ("", super.getDumpingAndReset()._2)
}

object ConstExpr {
  def apply(expr: String) = new ConstExpr(expr)
}

object Scalar {
  def apply(name: String, desc: String) = new Scalar(name, desc)
}

object Vector {
  def apply(n:Int, name: String, desc: String) = new Vector(n, name, desc)
}

object Distribution {
  def apply(min: Int, max: Int, bkt: Int, name: String, desc: String) = new Distribution(min, max, bkt, name, desc)
}

object Formula {
  def apply(node: NodeExpr, name: String, desc: String) = {
    val t = new Formula(name, desc)
    t := node
    t
  }
}

object AnoScalar {
  def apply() = new AnoScalar
}

object XstatsMgr {
  private val counters = new ListBuffer[Node]
  XSLog.registerCallerWithClock(collect)

  def addToFileRegisters {
    FileRegisters.add("xs_statistics.cpp", getCpp())
  }

  def register(n: Node) {
    val conflict = counters.exists(_.varname() == n.varname())
    if (conflict) {
      throw  new Exception(s"Statistics: ${n.varname()} has already exists")
    }
    counters.addOne(n)
  }

  private def getCpp() = {
    val cppvars: StringBuilder = new StringBuilder
    val dumpstr: StringBuilder = new StringBuilder
    val resetstr: StringBuilder = new StringBuilder
    val dpicfuncs: StringBuilder = new StringBuilder

    counters.foreach{c =>
      cppvars ++= c.getVarDef() + "\n"
      val t = c.getDumpingAndReset()
      dumpstr ++= t._1 + "\n"
      resetstr ++= t._2 + "\n"
      dpicfuncs ++= c.getDPIC() + "\n"
    }

    s"""
    |#include <cstdint>
    |#include <array>
    |#include <string>
    |#include <algorithm>
    |#include <numeric>
    |#include <cmath>
    |#include <iostream>
    |#include <sstream>
    |
    |${cppvars}
    |
    |extern "C" {
    |// call this function when dumping
    |void dpic_xs_statistics_dump() {
    |auto dout = std::stringstream();
    |dout << "[********** start dump Xstatistics **********]\\n";
    |${dumpstr}
    |std::cerr << dout.str();
    |
    |${resetstr}
    |}
    |
    |${dpicfuncs}
    |
    |}
    |
    """.stripMargin
  }

  private class XstatsDump extends HasDPICUtils {
    val io = IO(new Bundle{
      val clock = Input(Clock())
      val reset = Input(Reset())
      val en = Input(Bool())
    })
    init(io, false, false, "dpic_xs_statistics_dump")
  }

  def collect(ctrl: LogPerfIO, reset: Reset, clock: Clock) {
    val t = Module(new XstatsDump)
    t.io.clock := clock
    t.io.reset := reset
    t.io.en := ctrl.dump && ctrl.logEnable
  }
}