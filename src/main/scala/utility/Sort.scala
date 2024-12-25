/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open xVec Chip (BOSC)
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
* 
* Acknowledgement
* This implementation is inspired by several key materials:
* [1] https://github.com/OpenXiangShan/XiangShan/blob/d4ca2db4b3d45add495358a040aa2b9f26e40370/src/main/scala/xiangshan/mem/prefetch/L1PrefetchComponent.scala#L103
* [2] https://github.com/freechipsproject/chisel-bootcamp/blob/master/2.3_control_flow.ipynb
***************************************************************************************/

package utility

import chisel3._
import chisel3.util._

class TwithPtr[A <: Data, B <: CircularQueuePtr[B]](a: A, b: B) extends Bundle {
  val valid = Bool()
  val bits = a.cloneType
  val ptr = b.cloneType
}

object TwithPtr {
  /** Factory of class [[TwithPtr]]
    *
    * @param  valid Bool
    * @param  bits the origin Data
    * @param  ptr [[CircularQueuePtr]]
    * @return new hardware of [[TwithPtr]]
    */
  def apply[A <: Data, B <: CircularQueuePtr[B]](valid: Bool, bits: A, ptr: B): TwithPtr[A, B] = {
    val x = Wire(new TwithPtr[A, B](bits, ptr))
    x.valid := valid
    x.bits := bits
    x.ptr := ptr
    x
  }
}

object HwSort {
  /** Hardware Sort, a small combinational sorter that can sort up to 4 data by Ptr
    * 
    * The sort is stable. That is, elements that are invalid or equal (as determined by `cmp` function),
    * appear in the same order in the sorted sequence as in the original.
    * 
    * @tparam A default Data
    * @tparam B the type where "<" is defined, here it refers to [[CircularQueuePtr]]
    * @param  xVec the origin Vec data.
    * @param  cmp the ordering assumed on domain `B`
    * @return a Vec consisting of the elements of `xVec` sorted by `cmp`
    * 
    * @example {{{
    *   // Ascending sorting is default, i.e. the oldest comes first.
    *   val ascSorted = HwSort(VecInit(io.req.map { case x => TwithPtr(x.valid, x.bits, x.bits.uop.robIdx) }))
    *   
    *   // Descending sorting can be customized, i.e. the new one comes first.
    *   val desSorted = HwSort(
    *     VecInit(io.req.map { case x => TwithPtr(x.valid, x.bits, x.bits.uop.robIdx) }),
    *     [B <: CircularQueuePtr[B]](a: B, b: B) => { a > b }
    *   )
    * }}}
    */
  def apply[A <: Data, B <: CircularQueuePtr[B]](xVec: Vec[TwithPtr[A, B]], cmp: (B, B) => Bool): Vec[TwithPtr[A, B]] = {
    var size = xVec.length
    val res = WireInit(xVec)

    if (size == 1) {
      res := xVec
    } else if (size == 2) {
      // total ~20 ps
      // 1 is older than 0 (only when 0 and 1 are both valid)
      val swap = xVec(0).valid && xVec(1).valid && cmp(xVec(1).ptr, xVec(0).ptr)
      when(swap) {
        res(0) := xVec(1)
        res(1) := xVec(0)
      }
    } else if (size == 3) {
      // total ~40 ps
      val row0 = WireInit(xVec)
      val row1 = Wire(xVec.cloneType)
      val row2 = Wire(xVec.cloneType)
      val row3 = Wire(xVec.cloneType)

      val tmp0 = apply(VecInit(row0.slice(0, 2)), cmp)
      row1(0) := tmp0(0)
      row1(1) := tmp0(1)
      row1(2) := row0(2)
      val tmp1 = apply(VecInit(row1.slice(1, 3)), cmp)
      row2(0) := row1(0)
      row2(1) := tmp1(0)
      row2(2) := tmp1(1)
      val tmp2 = apply(VecInit(row2.slice(0, 2)), cmp)
      row3(0) := tmp2(0)
      row3(1) := tmp2(1)
      row3(2) := row2(2)

      res := row3

    } else if (size == 4){
      // total ~40 ps
      val row0 = WireInit(xVec)
      val row1 = Wire(xVec.cloneType)
      val row2 = Wire(xVec.cloneType)
      val row3 = Wire(xVec.cloneType)

      val tmp1_1 = apply(VecInit(row0(0), row0(1)), cmp)
      row1(0) := tmp1_1(0)
      row1(1) := tmp1_1(1)
      val tmp1_2 = apply(VecInit(row0(2), row0(3)), cmp)
      row1(2) := tmp1_2(0)
      row1(3) := tmp1_2(1)
      
      val tmp2_1 = apply(VecInit(row1(1), row1(2)), cmp)
      row2(1) := tmp2_1(0)
      row2(2) := tmp2_1(1)
      val tmp2_2 = apply(VecInit(row1(0), row1(3)), cmp)
      row2(0) := tmp2_2(0)
      row2(3) := tmp2_2(1)

      val tmp3_1 = apply(VecInit(row2(0), row2(1)), cmp)
      row3(1) := tmp3_1(0)
      row3(2) := tmp3_1(1)
      val tmp3_2 = apply(VecInit(row2(2), row2(3)), cmp)
      row3(0) := tmp3_2(0)
      row3(3) := tmp3_2(1)

      res := row3

    } else {
      require(false, f"xVec's size is ${size}, which is so big for hardware sort")
    }

    res
  }

  def apply[A <: Data, B <: CircularQueuePtr[B]](xVec: Vec[TwithPtr[A, B]]): Vec[TwithPtr[A, B]] = {
    def cmp(x: B, y: B): Bool = { x < y }
    apply(xVec, cmp)
  }

}
