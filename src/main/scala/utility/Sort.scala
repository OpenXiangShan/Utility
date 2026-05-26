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
*
* Acknowledgement
* This implementation is inspired by several key materials:
* [1] https://github.com/OpenXiangShan/XiangShan/blob/d4ca2db4b3d45add495358a040aa2b9f26e40370/src/main/scala/xiangshan/mem/prefetch/L1PrefetchComponent.scala#L103
* [2] https://github.com/freechipsproject/chisel-bootcamp/blob/master/2.3_control_flow.ipynb
***************************************************************************************/

package utility

import chisel3._
import chisel3.util._

class DataWithPtr[A <: Data, B <: CircularQueuePtr[B]](bitsCT: A, ptrCT: B) extends Bundle {
  val valid = Bool()
  val bits = bitsCT // CT: its chiselType
  val ptr = ptrCT
}

object DataWithPtr {
  /** Hardware Variable Factory of class [[DataWithPtr]]
    *
    * @param  valid Bool
    * @param  bits the origin Data
    * @param  ptr [[CircularQueuePtr]]
    * @return new hardware of [[DataWithPtr]]
    */
  def apply[A <: Data, B <: CircularQueuePtr[B]](valid: Bool, bits: A, ptr: B): DataWithPtr[A, B] = {
    val x = Wire(new DataWithPtr(chiselTypeOf(bits), chiselTypeOf(ptr)))
    x.valid := valid
    x.bits := bits
    x.ptr := ptr
    x
  }
}

object HwSort {
  /** Hardware Sort, a small combinational sorter that can sort up to 4 data by Ptr.
    * 
    * Valid elements are placed before invalid elements and sorted by `cmp`.
    * The order of equal elements and invalid elements is not guaranteed to be stable.
    * 
    * @tparam A default Data
    * @tparam B the type where "<" is defined, here it refers to [[CircularQueuePtr]]
    * @param  xVec the origin Vec data.
    * @param  cmp the ordering assumed on domain `B`, ascending sorting is default.
    * @return a Vec consisting of the elements of `xVec` sorted by `cmp`
    * 
    * @example {{{
    *   // Ascending sorting is default, i.e. the oldest comes first.
    *   val ascSorted = HwSort(VecInit(io.req.map { case x => DataWithPtr(x.valid, x.bits, x.bits.uop.robIdx) }))
    *   
    *   // Descending sorting can be customized, i.e. the new one comes first.
    *   // method1: implicitly set
    *   implicit val customCmp: (RobPtr, RobPtr) => Bool = (x, y) => x > y
    *   val desSorted1 = HwSort(VecInit(io.req.map { case x => DataWithPtr(x.valid, x.bits, x.bits.uop.robIdx) }))
    *   // method2: explicitly provide
    *   val desSorted2 = HwSort(VecInit(io.req.map { case x => DataWithPtr(x.valid, x.bits, x.bits.uop.robIdx) }))(
    *     (a, b) => a > b
    *   )
    * }}}
    */
  def apply[A <: Data, B <: CircularQueuePtr[B]](xVec: Vec[DataWithPtr[A, B]])
  (implicit cmp: (B, B) => Bool = (x: B, y: B) => x < y)
  : Vec[DataWithPtr[A, B]] = {
    var size = xVec.length
    val res = WireInit(xVec)

    def shouldSwap(a: DataWithPtr[A, B], b: DataWithPtr[A, B]): Bool = {
      !a.valid && b.valid || (a.valid && b.valid && cmp(b.ptr, a.ptr))
    }

    def assertSelectionMatrix(selMatrix: Vec[Vec[Bool]], n: Int): Unit = {
      for (k <- 0 until n) {
        assert(
          PopCount((0 until n).map(i => selMatrix(k)(i))) === 1.U,
          s"HwSort output slot $k must select exactly one input"
        )
      }
      for (i <- 0 until n) {
        val pickedCnt = PopCount((0 until n).map(k => selMatrix(k)(i)))
        when(xVec(i).valid) {
          assert(
            pickedCnt === 1.U,
            s"HwSort valid input $i must be selected exactly once"
          )
        }.otherwise {
          assert(
            pickedCnt <= 1.U,
            s"HwSort input $i is selected multiple times"
          )
        }
      }
    }

    if (size == 1) {
      res := xVec
    } else if (size == 2) {
      // total ~20 ps
      val swap = shouldSwap(xVec(0), xVec(1))
      when(swap) {
        res(0) := xVec(1)
        res(1) := xVec(0)
      }.otherwise {
        res(0) := xVec(0)
        res(1) := xVec(1)
      }
    } else if (size == 3) {
      // Sort 3 inputs with one parallel compare layer and a single payload select.
      def olderLH(i: Int, j: Int): Bool = {
        require(i < j)
        !shouldSwap(xVec(i), xVec(j))
      }

      val o01 = olderLH(0, 1)
      val o02 = olderLH(0, 2)
      val o12 = olderLH(1, 2)

      val rank = Wire(Vec(3, UInt(2.W)))
      val rankOH = Wire(Vec(3, UInt(3.W)))
      val selMatrix = Wire(Vec(3, Vec(3, Bool())))
      rank(0) := PopCount(Seq(!o01, !o02))
      rank(1) := PopCount(Seq(o01, !o12))
      rank(2) := PopCount(Seq(o02, o12))
      for (i <- 0 until 3) {
        rankOH(i) := UIntToOH(rank(i), 3)
      }

      for (k <- 0 until 3) {
        for (i <- 0 until 3) {
          selMatrix(k)(i) := rankOH(i)(k)
        }
        res(k) := Mux1H(selMatrix(k), xVec)
      }
      assertSelectionMatrix(selMatrix, 3)

    } else if (size == 4){
      def olderLH(i: Int, j: Int): Bool = {
        require(i < j)
        !shouldSwap(xVec(i), xVec(j))
      }

      val o01 = olderLH(0, 1)
      val o02 = olderLH(0, 2)
      val o03 = olderLH(0, 3)
      val o12 = olderLH(1, 2)
      val o13 = olderLH(1, 3)
      val o23 = olderLH(2, 3)

      val rank = Wire(Vec(4, UInt(2.W)))
      val rankOH = Wire(Vec(4, UInt(4.W)))
      val selMatrix = Wire(Vec(4, Vec(4, Bool())))
      rank(0) := PopCount(Seq(!o01, !o02, !o03))
      rank(1) := PopCount(Seq(o01, !o12, !o13))
      rank(2) := PopCount(Seq(o02, o12, !o23))
      rank(3) := PopCount(Seq(o03, o13, o23))
      for (i <- 0 until 4) {
        rankOH(i) := UIntToOH(rank(i), 4)
      }

      for (k <- 0 until 4) {
        for (i <- 0 until 4) {
          selMatrix(k)(i) := rankOH(i)(k)
        }
        res(k) := Mux1H(selMatrix(k), xVec)
      }
      assertSelectionMatrix(selMatrix, 4)

    } else {
      require(false, f"xVec's size is ${size}, which is so big for hardware sort")
    }

    res
  }

}
