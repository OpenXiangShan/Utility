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
import org.chipsalliance.cde.config.Parameters

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
    next := RegEnable(last, last.asUInt.orR || next.asUInt.orR)
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
  def apply[T <: Data](last: T, initOpt: Option[T] = None): T = {
    val next = Wire(chiselTypeOf(last))
    initOpt match {
      case Some(init) => next := RegEnable(last, init, last.asUInt =/= next.asUInt)
      case None => next := RegEnable(last, last.asUInt =/= next.asUInt)
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

class SegmentedAddr(_segments: Seq[Int]) {

  def this(f: Parameters => Seq[Int])(implicit p: Parameters) = this(f(p))

  val segments = _segments // (High, Lower ...)
  private var addr = UInt(segments.sum.W)

  // [High, Lower ...]
  private def segment(addr_in: UInt): Seq[UInt] = {
    segments.foldLeft((Seq[UInt](), addr_in.asBools)) { (acc, segment_length) =>
      ((acc._1 :+ VecInit(acc._2.takeRight(segment_length)).asUInt), acc._2.dropRight(segment_length))
    }._1
  }

  def getAddr(): UInt = {
    addr
  }
  def getAddrSegments(): Seq[UInt] = {
    segment(addr)
  }
  def fromSegments(seg: Seq[UInt]) = {
    this.addr = seg.reduce(Cat(_, _))
    this
  }
  def fromAddr(addr_in: UInt) = {
    this.addr = addr_in
    this
  }

  def compare(that: SegmentedAddr): Seq[Bool] = {
    this.getAddrSegments() zip that.getAddrSegments() map {
      case (l, r) => l =/= r
    }
  }

  def compare(that: Seq[UInt]): Seq[Bool] = {
    this.getAddrSegments() zip that map {
      case (l, r) => l =/= r
    }
  }
}

object SegmentedAddrNext {
  def apply(addr: SegmentedAddr): SegmentedAddr = {
    apply(addr, true.B, None)
  }

  def apply(addr: SegmentedAddr, fire: Bool, parentName: Option[String]): SegmentedAddr = {
    apply(addr.getAddr(), addr.segments, fire, parentName)
  }

  def apply(addr: UInt, segments: Seq[Int], fire: Bool, parentName: Option[String]): SegmentedAddr = {
    // Input wire, segmented
    val segmented = new SegmentedAddr(segments).fromAddr(addr).getAddrSegments()

    val modified = Wire(Vec(segmented.length, Bool()))

    val segmentedNext = segmented zip modified.zipWithIndex map {
      case (seg, (modified, idx)) => RegEnable(seg, modified && fire)
        .suggestName(s"${parentName.getOrElse("")}_seg_${idx}_value")
    }
    modified zip segmentedNext zip segmented map {
      case ((m, next), now) => m := next =/= now
    }
    modified.last := true.B // Assume lower part often changes

    val seg = new SegmentedAddr(segments).fromSegments(segmentedNext)
    if (parentName.isDefined) {
      val debug_addr = WireDefault(seg.getAddr()).suggestName(s"debug_${parentName.get}_addr")
      dontTouch(debug_addr)
    }

    seg
  }
}
