package utility

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._

// -----------------------------------------------------------------------------
// [Resize Modify]: 添加泛型约束，确保类型系统正确传播
// -----------------------------------------------------------------------------
class ResizeCircularQueuePtr[T <: ResizeCircularQueuePtr[T]](val entries: Int) extends Bundle {

  def this(f: Parameters => Int)(implicit p: Parameters) = this(f(p))

  val PTR_WIDTH = log2Up(entries)
  val flag = Bool()
  val value = UInt(PTR_WIDTH.W)
  
  // ---------------------------------------------------------------------------
  // [Resize Modify]: 新增动态尺寸控制信号
  // valid=true 时使用 bits 作为逻辑深度，否则使用 entries
  // ---------------------------------------------------------------------------
  val psize = Valid(UInt((PTR_WIDTH + 1).W))

  override def toPrintable: Printable = {
    p"$flag:$value"
  }

  final def +(v: UInt): T = {
    // -------------------------------------------------------------------------
    // [Resize Modify]: 计算有效大小
    // -------------------------------------------------------------------------
    val effectiveSize = Mux(psize.valid, psize.bits, entries.U)
    
    val new_ptr = Wire(this.asInstanceOf[T].cloneType)
    
    // [Resize Modify]: 仅当没有动态调整大小时，才允许使用静态 Pow2 优化
    // 否则必须走通用的取模/减法逻辑
    if(isPow2(entries)){
        // 这里的逻辑稍微复杂一点：如果原本是 Pow2 且当前未 resize，走快速路径
        // 如果 resize 了，哪怕 resize 后也是 Pow2，为了硬件统一，走下面的通用路径
        val useFastPath = !psize.valid
        
        // 快速路径逻辑 (原版)
        val fast_ptr = (Cat(this.flag, this.value) + v).asTypeOf(new_ptr)
        
        // 通用路径逻辑 (适配 Pow2 entries 被 resize 的情况)
        // 类似于原版非 Pow2 的处理，但是针对 effectiveSize
        val raw_sum = this.value +& v
        val diff = Cat(0.U(1.W), raw_sum).asSInt - Cat(0.U(1.W), effectiveSize).asSInt
        val reverse_flag = diff >= 0.S
        
        new_ptr.flag := Mux(useFastPath, fast_ptr.flag, Mux(reverse_flag, !this.flag, this.flag))
        new_ptr.value := Mux(useFastPath, fast_ptr.value, Mux(reverse_flag, diff.asUInt, raw_sum))

    } else {
      // 原版非 Pow2 逻辑的扩展，将 entries.U 替换为 effectiveSize
      val new_value = this.value +& v
      val diff = Cat(0.U(1.W), new_value).asSInt - Cat(0.U(1.W), effectiveSize).asSInt
      val reverse_flag = diff >= 0.S
      new_ptr.flag := Mux(reverse_flag, !this.flag, this.flag)
      new_ptr.value := Mux(reverse_flag,
        diff.asUInt,
        new_value
      )
    }
    
    // [Resize Modify]: 必须传递 psize 配置
    new_ptr.psize := this.psize
    new_ptr
  }

  final def -(v: UInt): T = {
    // [Resize Modify]: 减法逻辑依赖加法，需使用 effectiveSize 进行补码计算
    val effectiveSize = Mux(psize.valid, psize.bits, entries.U)
    val flipped_new_ptr = this + (effectiveSize - v)
    
    val new_ptr = Wire(this.asInstanceOf[T].cloneType)
    new_ptr.flag := !flipped_new_ptr.flag
    new_ptr.value := flipped_new_ptr.value
    // [Resize Modify]: 传递 psize
    new_ptr.psize := this.psize
    new_ptr
  }

  // ---------------------------------------------------------------------------
  // [Resize Modify]: 新增 resize 方法，用于初始化或重置队列大小时生成新指针
  // ---------------------------------------------------------------------------
  def resize(v: UInt, p: UInt): T = {
    val new_ptr = Wire(this.asInstanceOf[T].cloneType)
    new_ptr.flag := false.B
    new_ptr.value := v
    new_ptr.psize.valid := true.B
    new_ptr.psize.bits := p
    new_ptr
  }

  final def === (that: T): Bool = this.asUInt === that.asUInt

  final def =/= (that: T): Bool = this.asUInt =/= that.asUInt

  // 比较逻辑 (>, <, >=, <=) 依赖 flag 和 value 的相对关系
  // 只要 + 和 - 维护好了 flag/value 在 effectiveSize 下的正确性，
  // 这些比较逻辑不需要修改。
  final def > (that: T): Bool = {
    val differentFlag = this.flag ^ that.flag
    val compare = this.value > that.value
    differentFlag ^ compare
  }

  final def < (that: T): Bool = {
    val differentFlag = this.flag ^ that.flag
    val compare = this.value < that.value
    differentFlag ^ compare
  }

  final def >= (that: T): Bool = {
    val differentFlag = this.flag ^ that.flag
    val compare = this.value >= that.value
    differentFlag ^ compare
  }

  final def <= (that: T): Bool = {
    val differentFlag = this.flag ^ that.flag
    val compare = this.value <= that.value
    differentFlag ^ compare
  }

  def toOH: UInt = UIntToOH(value, entries)
}

trait HasResizeCircularQueuePtrHelper {

  def isEmpty[T <: ResizeCircularQueuePtr[T]](enq_ptr: T, deq_ptr: T): Bool = {
    enq_ptr === deq_ptr
  }

  // [Resize Modify Analysis]:
  // 香山原版 isFull 定义：flag 不同且 value 相同。
  // 这意味着队列可以被填满 entries 个元素 (Full Utilization)。
  // 即使在 resize 模式下，只要 + 逻辑保证了 value == effectiveSize 时发生回绕，
  // 这个 isFull 逻辑依然成立。无需修改。
  def isFull[T <: ResizeCircularQueuePtr[T]](enq_ptr: T, deq_ptr: T): Bool = {
    (enq_ptr.flag =/= deq_ptr.flag) && (enq_ptr.value === deq_ptr.value)
  }

  def distanceBetween[T <: ResizeCircularQueuePtr[T]](enq_ptr: T, deq_ptr: T): UInt = {
    assert(enq_ptr.entries == deq_ptr.entries)
    
    // [Resize Modify]: 计算距离必须基于 effectiveSize
    val effectiveSize = Mux(enq_ptr.psize.valid, enq_ptr.psize.bits, enq_ptr.entries.U)
    
    Mux(enq_ptr.flag === deq_ptr.flag,
      enq_ptr.value - deq_ptr.value,
      effectiveSize + enq_ptr.value - deq_ptr.value) // 原代码 entries.U -> effectiveSize
  }

  def hasFreeEntries[T <: ResizeCircularQueuePtr[T]](enq_ptr: T, deq_ptr: T): UInt = {
    val free_deq_ptr = enq_ptr
    val free_enq_ptr = WireInit(deq_ptr)
    free_enq_ptr.flag := !deq_ptr.flag
    // distanceBetween 已经修改为支持 effectiveSize，此处无需修改
    distanceBetween(free_enq_ptr, free_deq_ptr)
  }

  def isAfter[T <: ResizeCircularQueuePtr[T]](left: T, right: T): Bool = left > right

  def isBefore[T <: ResizeCircularQueuePtr[T]](left: T, right: T): Bool = left < right

  def isNotAfter[T <: ResizeCircularQueuePtr[T]](left: T, right: T): Bool = left <= right

  def isNotBefore[T <: ResizeCircularQueuePtr[T]](left: T, right: T): Bool = left >= right
}