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
  
  // It is useless to clockgate only one bit, so change to RegNext here
  def apply(next: Bool, init: Bool = false.B): Bool = {
    val last = WireInit(false.B)
    last := RegNext(next, init)
    last
  }

  def apply(last: Vec[Bool]): Vec[Bool] = {
    val next = VecInit(Seq.fill(last.size)(false.B))
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
    val nextVec = WireInit(0.U.asTypeOf(lastVec))
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
    val next = WireInit(0.U.asTypeOf(last))
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

class SegmentedAddr(_segments: Seq[Int]) {

  def this(f: Parameters => Seq[Int])(implicit p: Parameters) = this(f(p))

  val segments = _segments // (High, Lower ...)
  private var addr = UInt(segments.sum.W)

  // [High, Lower ...]
  private def segment(addrIn: UInt): Seq[UInt] = {
    segments.foldLeft((Seq[UInt](), addrIn.asBools)) { (acc, segment_length) =>
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
  def fromAddr(addrIn: UInt) = {
    this.addr = addrIn
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

    val segmentedNext = segments zip segmented zip modified.zipWithIndex map {
      case ((segLength, seg), (modified, idx)) =>
      // Must init here to avoid X state
      RegEnable(seg, 0.U(segLength.W), modified && fire)
        .suggestName(s"${parentName.getOrElse("")}_seg_${idx}_value")
    }
    modified zip segmentedNext zip segmented map {
      case ((m, next), now) => m := next =/= now
    }
    modified.last := true.B // Assume lower part often changes

    val seg = new SegmentedAddr(segments).fromSegments(segmentedNext)
    if (parentName.isDefined) {
      val debug_addr = WireDefault(seg.getAddr()).suggestName(s"debug_${parentName.get}_addr")
      val debug_modified = modified.suggestName(s"debug_${parentName.get}_modified")
      dontTouch(debug_addr)
      dontTouch(debug_modified)
    }

    seg
  }
}
