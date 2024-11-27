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

object SubVec{
  // Rem: SubSeq = (Seq % Modulus)(Remainder)
  // NOTE: must be divisible
  def getRem[T <: Data](in: Seq[T], mod: Int, rem: Int): Seq[T] = {
    require(in.size % mod == 0)
    require(mod > rem)
    (0 until in.size / mod).map(i => {in(mod * i + rem)})
  }
  def getRem[T <: Data](in: Seq[T], mod: Int): Seq[Seq[T]] = {
    (0 until mod).map { rem => getRem(in, mod, rem) }
  }

  // RemWithExpansion: SubSeqWithExpansion = (Seq % Modulus expanded)(Remainder)
  def getRemWithExpansion[T <: Data](in: Seq[T], mod: Int, rem: Int): Seq[T] = {
    require(mod > rem)
    val subSize = in.size / mod + 1
    (0 until subSize).map {i =>
      val idx = mod * i + rem
      if (idx < in.size) in(idx) else 0.U.asTypeOf(in(0))
    }
  }
  def getRemWithExpansion[T <: Data](in: Seq[T], mod: Int): Seq[Seq[T]] = {
    (0 until mod).map { rem => getRemWithExpansion(in, mod, rem) }
  }

  // MaskRem: MaskSeq = RemMask & Seq(RemIdx)
  // NOTE: logical size is big
  def getMaskRem(in: Vec[Bool], mod: Int, rem: Int): Vec[Bool] = {
    require(mod > rem)
    val out = WireInit(0.U.asTypeOf(in))
    (0 until in.size).map(i => { if(i % mod == rem) out(i) := in(i) else out(i) := false.B})
    out
  }
  def getMaskRem(in: Vec[Bool], mod: Int): Vec[Vec[Bool]] = {
    VecInit((0 until mod).map(rem => getMaskRem(in, mod, rem)))
  }

}
