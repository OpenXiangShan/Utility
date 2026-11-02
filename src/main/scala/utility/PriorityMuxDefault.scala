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

object PriorityMuxDefault {
  def apply[T <: Data](in: Seq[(Bool, T)], default: T): T = {
    in.size match {
      case 1=>
        Mux(in.head._1, in.head._2, default)
      case _ =>
        Mux(in.head._1, in.head._2, PriorityMuxDefault(in.tail, default))
    }
  }
}

object PriorityEncoderDefault {
  def apply(in: Vec[Bool], default: UInt)(implicit source: chisel3.experimental.SourceInfo): UInt = {
    if (SimulationPrimitives.selected("PriorityEncoderDefault.Vec", in.length, "distributed")) {
      require(in.nonEmpty)
      val packed = chisel3.util.Cat(in.reverse)
      Mux(packed.orR, SimulationPrimitives.index(packed), default)
    } else PriorityMuxDefault(in.zipWithIndex.map(x => x._1 -> x._2.U), default)
  }
  def apply(in: Seq[Bool], default: UInt)(implicit source: chisel3.experimental.SourceInfo): UInt = {
    if (SimulationPrimitives.selected("PriorityEncoderDefault", in.size, "distributed")) {
      require(in.nonEmpty)
      val packed = chisel3.util.Cat(in.reverse)
      Mux(packed.orR, SimulationPrimitives.index(packed), default)
    } else PriorityMuxDefault(in.zipWithIndex.map(x => x._1 -> x._2.U), default)
  }
}

object PriorityMuxWithFlag {
  def apply[T <: Data](in: Seq[(Bool, T)]): (T, Bool) = {
    in.size match {
      case 1 =>
        (in.head._2, in.head._1)
      case _ =>
        val (d_tail, f_tail) = PriorityMuxWithFlag(in.tail)
        val d_head = in.head._2
        val f_head = in.head._1
        (Mux(f_head, d_head, d_tail), f_head || f_tail)
    }
  }
}

object PriorityEncoderWithFlag {
  def apply(in: Vec[Bool])(implicit source: chisel3.experimental.SourceInfo): (UInt, Bool) = {
    if (SimulationPrimitives.selected("PriorityEncoderWithFlag.Vec", in.length, "distributed")) {
      require(in.nonEmpty)
      val packed = chisel3.util.Cat(in.reverse)
      (SimulationPrimitives.index(packed), packed.orR)
    } else PriorityMuxWithFlag(in.zipWithIndex.map(x => x._1 -> x._2.U))
  }
  def apply(in: Seq[Bool])(implicit source: chisel3.experimental.SourceInfo): (UInt, Bool) = {
    if (SimulationPrimitives.selected("PriorityEncoderWithFlag.Seq", in.size, "distributed")) {
      require(in.nonEmpty)
      val packed = chisel3.util.Cat(in.reverse)
      (SimulationPrimitives.index(packed), packed.orR)
    } else PriorityMuxWithFlag(in.zipWithIndex.map(x => x._1 -> x._2.U))
  }
  def apply(in: Bits)(implicit source: chisel3.experimental.SourceInfo): (UInt, Bool) = {
    if (SimulationPrimitives.selected("PriorityEncoderWithFlag.Bits", in.getWidth, "packed")) {
      val packed = in.asUInt
      (SimulationPrimitives.index(packed), packed.orR)
    } else PriorityEncoderWithFlag(in.asBools)
  }
}
