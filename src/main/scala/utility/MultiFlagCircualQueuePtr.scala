/***************************************************************************************
 * Copyright (c) 2024-2026 Beijing Institute of Open Source Chip (BOSC)
 * Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          https://license.coscl.org.cn/MulanPSL2
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

package utility

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._

class MultiFlagCircularQueuePtr[T <: MultiFlagCircularQueuePtr[T]](val entries: Int, val multiple: Int) extends Bundle {
  def this(f: Parameters => (Int, Int))(implicit p: Parameters) = this(f(p)._1, f(p)._2)
  require(multiple > 1)

  val multiEntries = multiple * entries
  val PTR_WIDTH = log2Up(entries)
  val FLAG_WIDTH = log2Up(multiple)
  val first_flag = Bool()
  val second_flag = UInt(FLAG_WIDTH.W)
  val value = UInt(PTR_WIDTH.W)
  def operation_value: UInt = Cat(second_flag, value)
  def operation_flag: UInt = Cat(first_flag, second_flag)

  def operation_flag_=(v: UInt): Unit = {
    require(v.getWidth == (FLAG_WIDTH + 1))
    first_flag := v(FLAG_WIDTH)
    second_flag := v(FLAG_WIDTH - 1, 0)
  }
  def operation_value_=(v: UInt): Unit = {
    require(v.getWidth == (PTR_WIDTH + FLAG_WIDTH))
    second_flag := v(v.getWidth - 1, PTR_WIDTH)
    value := v(PTR_WIDTH - 1, 0)
  }

  override def toPrintable: Printable = {
    p"$first_flag:$second_flag:$value"
  }

  final def +(v: UInt): T = {
    val new_ptr = Wire(this.asInstanceOf[T].cloneType)
    if(isPow2(multiEntries)){
      new_ptr := (Cat(this.first_flag, this.operation_value) + v).asTypeOf(new_ptr)
    } else {
      val new_value = this.value +& v
      val wrap_width = log2Ceil(multiEntries + 1)
      val wrap_hit = (0 to multiple).map(i =>
        if (i == multiple) true.B else new_value < ((i + 1) * entries).U
      )
      val wrap_count = PriorityEncoder(wrap_hit)
      val wrap_base = PriorityMux(wrap_hit, (0 to multiple).map(i => (i * entries).U(wrap_width.W)))
      val next_value = new_value - wrap_base
      if (isPow2(multiple)) {
        val next_flag = this.operation_flag + wrap_count
        new_ptr.operation_flag = next_flag
      } else {
        val next_flag_index = Mux(
          this.first_flag,
          multiple.U((FLAG_WIDTH + 1).W) + this.second_flag,
          this.second_flag
        ) + wrap_count
        val wrapped_next_flag_index = Mux(
          next_flag_index >= (multiple * 2).U,
          next_flag_index - (multiple * 2).U,
          next_flag_index
        )
        new_ptr.first_flag := wrapped_next_flag_index >= multiple.U
        new_ptr.second_flag := Mux(
          wrapped_next_flag_index >= multiple.U,
          wrapped_next_flag_index - multiple.U,
          wrapped_next_flag_index
        )(FLAG_WIDTH - 1, 0)
      }
      new_ptr.value := next_value(PTR_WIDTH - 1, 0)
    }
    new_ptr
  }

  final def -(v: UInt): T = {
    val flipped_new_ptr = this + (this.multiEntries.U - v)
    val new_ptr = Wire(this.asInstanceOf[T].cloneType)
    new_ptr.first_flag := !flipped_new_ptr.first_flag
    new_ptr.operation_value = flipped_new_ptr.operation_value
    new_ptr
  }

  final def === (that: T): Bool = this.asUInt === that.asUInt

  final def =/= (that: T): Bool = this.asUInt =/= that.asUInt

  final def > (that: T): Bool = {
    val differentFlag = this.first_flag ^ that.first_flag
    val compare = this.operation_value > that.operation_value
    differentFlag ^ compare
  }

  final def < (that: T): Bool = {
    val differentFlag = this.first_flag ^ that.first_flag
    val compare = this.operation_value < that.operation_value
    differentFlag ^ compare
  }

  final def >= (that: T): Bool = {
    val differentFlag = this.first_flag ^ that.first_flag
    val compare = this.operation_value >= that.operation_value
    differentFlag ^ compare
  }

  final def <= (that: T): Bool = {
    val differentFlag = this.first_flag ^ that.first_flag
    val compare = this.operation_value <= that.operation_value
    differentFlag ^ compare
  }

  def toOH: UInt = UIntToOH(value, entries)
}

trait HasMultiFlagCircularQueuePtrHelper {

  def isEmpty[T <: MultiFlagCircularQueuePtr[T]](enq_ptr: T, deq_ptr: T): Bool = {
    enq_ptr === deq_ptr
  }

  def isFull[T <: MultiFlagCircularQueuePtr[T]](enq_ptr: T, deq_ptr: T): Bool = {
    (enq_ptr.first_flag =/= deq_ptr.first_flag) && (enq_ptr.operation_value === deq_ptr.operation_value)
  }

  def distanceBetween[T <: MultiFlagCircularQueuePtr[T]](enq_ptr: T, deq_ptr: T): UInt = {
    assert(enq_ptr.multiEntries == deq_ptr.multiEntries)
    assert(enq_ptr.entries == deq_ptr.entries)
    if (isPow2(enq_ptr.entries)) {
      Mux(enq_ptr.first_flag === deq_ptr.first_flag,
        enq_ptr.operation_value - deq_ptr.operation_value,
        enq_ptr.multiEntries.U + enq_ptr.operation_value - deq_ptr.operation_value)
    } else {
      val flagEntries = enq_ptr.multiple * 2
      val flagDistance = if (isPow2(enq_ptr.multiple)) {
        enq_ptr.operation_flag - deq_ptr.operation_flag
      } else {
        def flagIndex(ptr: T): UInt = Mux(
          ptr.first_flag,
          ptr.multiple.U((ptr.FLAG_WIDTH + 1).W) + ptr.second_flag,
          ptr.second_flag
        )
        val enqFlagIndex = flagIndex(enq_ptr)
        val deqFlagIndex = flagIndex(deq_ptr)
        Mux(
          enqFlagIndex >= deqFlagIndex,
          enqFlagIndex - deqFlagIndex,
          flagEntries.U + enqFlagIndex - deqFlagIndex
        )
      }
      val flagDistanceBase = Mux1H(
        (0 until flagEntries).map(i => flagDistance === i.U),
        (0 until flagEntries).map(i => (i * enq_ptr.entries).U)
      )
      (flagDistanceBase +& enq_ptr.value - deq_ptr.value)
    }
  }

  def hasFreeEntries[T <: MultiFlagCircularQueuePtr[T]](enq_ptr: T, deq_ptr: T): UInt = {
    val free_deq_ptr = enq_ptr
    val free_enq_ptr = WireInit(deq_ptr)
    free_enq_ptr.first_flag := !deq_ptr.first_flag
    distanceBetween(free_enq_ptr, free_deq_ptr)
  }

  def withInPhysicalQueue[T <: MultiFlagCircularQueuePtr[T]](startPtr: T, thisPtr: T, entrySize: Int): Bool = {
    val endPtr = startPtr + entrySize.U
    thisPtr >= startPtr && thisPtr < endPtr
  }

  def isAfter[T <: MultiFlagCircularQueuePtr[T]](left: T, right: T): Bool = left > right

  def isBefore[T <: MultiFlagCircularQueuePtr[T]](left: T, right: T): Bool = left < right

  def isNotAfter[T <: MultiFlagCircularQueuePtr[T]](left: T, right: T): Bool = left <= right

  def isNotBefore[T <: MultiFlagCircularQueuePtr[T]](left: T, right: T): Bool = left >= right
}
