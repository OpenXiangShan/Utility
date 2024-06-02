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

object GatedValidRegNext {
  // 3 is the default minimal width of EDA inserted clock gating cells.
  // so using `GatedValidRegNext` to signals whoes width is less than 3 may not help.
  def apply(next: Bool, init: Bool = false.B): Bool = {
    val last = Wire(Bool())
    last := RegEnable(next, init, next || last)
    last
  }

  def apply(last: Vec[Bool]): Vec[Bool] = {
    val next = Wire(chiselTypeOf(last))
    next := RegEnable(last, VecInit(Seq.fill(last.size)(false.B)), last.asUInt =/= next.asUInt)
    next
  }
}

object GatedValidRegNextN {
  def apply(in: Bool, n: Int, initOpt: Option[Bool] = None): Bool = {
    (0 until n).foldLeft(in){
      (prev, _) =>
        initOpt match {
          case Some(init) => GatedValidRegNext(prev, init)
          case None => GatedValidRegNext(prev)
        }
    }
  }
}

object GatedRegNext{
  // Vec can be judged and assigned one by one
  def regEnableVec[T <: Data](lastVec: Vec[T], initOptVec: Option[Vec[T]]): Vec[T] = {
    val nextVec = Wire(chiselTypeOf(lastVec))
    for (i <- 0 until lastVec.length) {
      initOptVec match {
        case Some(initVec) => nextVec(i) := RegEnable(lastVec(i), initVec(i), lastVec(i).asUInt =/= nextVec(i).asUInt)
        case None => nextVec(i) := RegEnable(lastVec(i), 0.U.asTypeOf(lastVec(i)), lastVec(i).asUInt =/= nextVec(i).asUInt)
      }
    }
    nextVec
  }

  // NOTICE: The larger Data width , the longger time of =/= operations, which may lead to timing violations.
  // Callers need to consider timing requirements themselves.
  def apply[T <: Data](last: T, initOpt: Option[T] = None): T = {
    val next = Wire(chiselTypeOf(last))
    last match {
      case v: Vec[_] =>
        next := regEnableVec(v.asInstanceOf[Vec[T]], initOpt.map(_.asInstanceOf[Vec[T]]))
      case _ =>
        initOpt match {
          case Some(init) => next := RegEnable(last, init, last.asUInt =/= next.asUInt)
          case None => next := RegEnable(last, 0.U.asTypeOf(last), last.asUInt =/= next.asUInt)
        }
    }
    next
  }
}

object GatedRegNextN {
  def apply[T <: Data](in: T, n: Int, initOpt: Option[T] = None): T = {
    (0 until n).foldLeft(in){
      (prev, _) => GatedRegNext(prev, initOpt)
    }
  }
}
