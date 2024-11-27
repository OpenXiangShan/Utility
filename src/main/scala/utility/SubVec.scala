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
import freechips.rocketchip.util.SeqToAugmentedSeq

object SubVec{
  // Rem: SubSeq = (Seq % Modulus)(Remainder)
  // NOTE: must be divisible
  def getRem[T <: Data](in: Seq[T], mod: Int, rem: Int): Seq[T] = {
    require(in.size % mod == 0)
    (0 until in.size / mod).map(i => {in(mod * i + rem)})
  }
  def getRems[T <: Data](in: Seq[T], mod: Int): Seq[Seq[T]] = {
    require(in.size % mod == 0)
    (0 until mod).map { rem => getRem(in, mod, rem) }
  }

  // RemWithExpand: SubSeqWithExpand = (Seq % Modulus expanded)(Remainder)
  def getRemWithExpand(in: Seq[Bool], mod: Int, rem: Int): Seq[Bool] = {
    val subSize = in.size / mod + 1
    (0 until subSize).map {i =>
      val idx = mod * i + rem
      if (idx < in.size) in(idx) else false.B
    }
  }
  def getRemWithExpands(in: Seq[Bool], mod: Int): Seq[Seq[Bool]] = {
    (0 until mod).map { rem => getRemWithExpand(in, mod, rem) }
  }

  // MaskRem: SubSeq = RemMask & Seq(RemIdx)
  // NOTE: logical size  big
  def getMaskRem(in: Vec[Bool], mod: Int, rem: Int): Vec[Bool] = {
    val out = WireInit(0.U.asTypeOf(in))
    (0 until in.size).map(i => { if(i % mod == rem) out(i) := in(i) else out(i) := false.B})
    out
  }
  def getMaskRems(in: Vec[Bool], mod: Int): Vec[Vec[Bool]] = {
    VecInit((0 until mod).map(rem => getMaskRem(in, mod, rem)))
  }

}