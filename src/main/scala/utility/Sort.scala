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

    /**
      * shouldSwap(a, b) == true means a and b should be swapped,
      * i.e., b should be placed before a (b has priority / is considered "older").
      *
      * Cases:
      * - a.valid=1, b.valid=1: decided by cmp(b.ptr, a.ptr) (compare ptrs). If cmp(b,a) == true,
      *   then b is older and we swap; otherwise no swap.
      * - a.valid=1, b.valid=0: a is valid and b is invalid -> no swap (valid elements have priority).
      * - a.valid=0, b.valid=1: a is invalid and b is valid -> swap (place valid before invalid).
      * - a.valid=0, b.valid=0: both invalid -> no swap (this comparison does not prioritize either).
      */
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
    } else if (size >= 3 && size <= 4) {
      
      // olderLH(i,j) indicates that input i is older than input j (i should come before j).
      // - When both are invalid (valid=0), the input with the smaller index is considered older (tie-break).
      // - When one is valid and the other is invalid, the valid one is considered older (has priority).
      def olderLH(i: Int, j: Int): Bool = {
        require(i < j)
        !shouldSwap(xVec(i), xVec(j))
      }

      val rankWidth = log2Ceil(size)
      val rank = Wire(Vec(size, UInt(rankWidth.W)))
      val rankOH = Wire(Vec(size, UInt(size.W)))
      val selMatrix = Wire(Vec(size, Vec(size, Bool())))

      // ageMatrix(i)(j) := true if input j is older than input i
      val ageMatrix = Wire(Vec(size, Vec(size, Bool())))
      for (i <- 0 until size) {
        for (j <- 0 until size) {
          if (i == j) {
            ageMatrix(i)(j) := false.B
          } else if (j < i) {
            ageMatrix(i)(j) := olderLH(j, i)
          } else {
            ageMatrix(i)(j) := !olderLH(i, j)
          }
        }
      }

      // rank(i) = number of inputs older than input i (i's target output index)
      for (i <- 0 until size) {
        rank(i) := PopCount((0 until size).map(j => ageMatrix(i)(j)))
      }

      // rankMatrix: per-input one-hot encoding of its rank (input-as-row, output-as-col)
      val rankMatrix = Wire(Vec(size, UInt(size.W)))
      for (i <- 0 until size) {
        rankMatrix(i) := UIntToOH(rank(i), size)
      }

      for (k <- 0 until size) {
        for (i <- 0 until size) {
          selMatrix(k)(i) := rankMatrix(i)(k)
        }
        res(k) := Mux1H(selMatrix(k), xVec)
      }
      assertSelectionMatrix(selMatrix, size)

    } else {
      require(false, f"xVec's size is ${size}, which is so big for hardware sort")
    }

    res
  }

}
