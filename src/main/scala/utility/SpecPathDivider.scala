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
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

import scala.math.min

// Input: payload with agePtr, such as RobPtr
// OtherInput:
//   1. redirectPtr to set wrongPath
//   2. CommitPtr to set archPath
// Output: payload with archResult and its input timestamp
class SpecPathDivider[T <: Data, C <: CircularQueuePtr[C]](
  val gen: T,
  val ptrGen: C,
  val inWidth: Int,
  val entries: Int,
  val ageFilter: Boolean = true
)(implicit val p: Parameters) extends Module
  with HasCircularQueuePtrHelper {
  val outWidth = min(inWidth * 2, entries)
  require(inWidth < entries)

  val io = IO(new Bundle {
    val in = Input(Vec(inWidth, Valid(gen)))

    val inAgePtr = Input(Vec(inWidth, ptrGen))
    val redirectAgePtr = Input(Valid(ptrGen))
    val commitAgePtr = Input(Valid(ptrGen))

    val out = Output(Vec(outWidth, Valid(gen)))
    val outIsArch = Output(Vec(outWidth, Bool()))
    val outStamp = Output(Vec(outWidth, UInt(64.W)))
  })

  val inputStamp = RegInit(0.U(64.W))
  inputStamp := inputStamp + 1.U

  val s_idle :: s_specPath :: s_wrongPath :: s_archPath :: Nil = Enum(4)

  val payloadQueue = Reg(Vec(entries, gen))
  val stateQueue = RegInit(VecInit(Seq.fill(entries)(s_idle)))
  val validQueue = VecInit(stateQueue.map(state => state =/= s_idle))
  val stampQueue = Reg(Vec(entries, UInt(64.W)))
  val agePtrQueue = Reg(Vec(entries, ptrGen))

  val readyInVec = VecInit(validQueue.map(!_))
  val readyOutVec = VecInit(stateQueue.map {
    case state => (state === s_wrongPath) || (state === s_archPath)
  })

  val inPtrVec = SelectFirstN(readyInVec, inWidth)
  val outPtrVec = SelectFirstN(readyOutVec, outWidth)
  require(inPtrVec.head.getWidth == log2Ceil(entries))
  require(outPtrVec.head.getWidth == log2Ceil(entries))

  val inReadyVec = inPtrVec.map(readyInVec(_))
  val outReadyVec = outPtrVec.map(readyOutVec(_))

  // Redirect Update
  when (io.redirectAgePtr.valid) {
    for (i <- 0 until entries) {
      when (validQueue(i) && isBefore(io.redirectAgePtr.bits, agePtrQueue(i))) {
        stateQueue(i) := s_wrongPath
        // XSError(stateQueue(i) === s_archPath, s"ArchPath $i is redirected")
      }
    }
  }

  // Commit Update
  when (io.commitAgePtr.valid) {
    for (i <- 0 until entries) {
      when (validQueue(i) && isAfter(io.commitAgePtr.bits, agePtrQueue(i))) {
        stateQueue(i) := s_archPath
        // XSError(stateQueue(i) === s_wrongPath, s"WrongPath $i is committed")
      }
    }
  }
  // XSError((io.redirectAgePtr.valid && io.commitAgePtr.valid && isBefore(io.redirectAgePtr.bits, io.commitAgePtr.bits)), "RedirectPtr Older than CommitPtr")

  // IO Connections
  (0 until inWidth).foreach { i =>
    val redirected = io.redirectAgePtr.valid && isBefore(io.redirectAgePtr.bits, io.inAgePtr(i))
    val committed = io.commitAgePtr.valid && isAfter(io.commitAgePtr.bits, io.inAgePtr(i))
    val ageFiltered = if (!ageFilter) false.B
      else validQueue.zip(agePtrQueue).map { case (valid, agePtr) =>
          valid && agePtr === io.inAgePtr(i)
        }.reduce(_ || _)

    when (io.in(i).valid && !ageFiltered) {
      payloadQueue(inPtrVec(i)) := io.in(i).bits
      stampQueue(inPtrVec(i)) := inputStamp
      agePtrQueue(inPtrVec(i)) := io.inAgePtr(i)
      stateQueue(inPtrVec(i)) :=
        Mux(committed, s_archPath,
        Mux(redirected, s_wrongPath, s_specPath))
      XSError(!inReadyVec(i), "SpecPathDivider: input " + i + " is not ready")
    }
  }

  (0 until outWidth).foreach { i =>
    io.out(i).valid := outReadyVec(i)
    io.out(i).bits := payloadQueue(outPtrVec(i))
    io.outIsArch(i) := stateQueue(outPtrVec(i)) === s_archPath
    io.outStamp(i) := stampQueue(outPtrVec(i))

    when (io.out(i).valid) {
      stateQueue(outPtrVec(i)) := s_idle
    }
  }
}

object ChiselDBWithSpecDivide {
  def apply[T <: Data, C <: CircularQueuePtr[C]](
    tableName: String,
    siteName: String,
    entryNum: Int,
    in: Vec[Valid[T]],
    inAgePtr: Vec[C],
    redirectAgePtr: Valid[C],
    commitAgePtr: Valid[C],
    clock: Clock,
    reset: Reset,
    basicDB: Boolean = false
  )(implicit p: Parameters): Unit = {
    val divider = Module(new SpecPathDivider[T, C](chiselTypeOf(in.head.bits), chiselTypeOf(inAgePtr.head), in.length, entryNum))
    divider.io.in := in
    divider.io.inAgePtr := inAgePtr
    divider.io.redirectAgePtr := redirectAgePtr
    divider.io.commitAgePtr := commitAgePtr

    class InBundleWithSpecDivide extends Bundle {
      val pl = chiselTypeOf(in.head.bits)
      val isArch = Bool()
    }
    val dbData = Wire(Vec(divider.io.out.length, Valid(new InBundleWithSpecDivide())))
    (0 until dbData.length).foreach { i =>
      dbData(i).bits.pl := divider.io.out(i).bits
      dbData(i).bits.isArch := divider.io.outIsArch(i)
      dbData(i).valid := divider.io.out(i).valid
    }

    val table = ChiselDB.createTable(tableName, new InBundleWithSpecDivide, basicDB = basicDB)
    (0 until divider.io.out.length).foreach { i =>
      table.log(
        dbData(i).bits,
        dbData(i).valid,
        siteName,
        divider.io.outStamp(i),
        clock, reset)
    }
  }
}

object ChiselMapWithSpecDivide {
  def apply[T <: Data, V <: Data, C <: CircularQueuePtr[C]](
    tableName: String,
    siteName: String,
    entryNum: Int,
    inKey: Vec[T],
    inValue: Vec[V],
    inValid: Vec[Bool],
    inAgePtr: Vec[C],
    redirectAgePtr: Valid[C],
    commitAgePtr: Valid[C],
    clock: Clock,
    reset: Reset,
    basicDB: Boolean = false
  )(implicit p: Parameters): Unit = {
    class PayLoadType extends Bundle {
      val key = chiselTypeOf(inKey.head)
      val value = chiselTypeOf(inValue.head)
    }
    val divider = Module(new SpecPathDivider[PayLoadType, C]
      (new PayLoadType, chiselTypeOf(inAgePtr.head), inKey.length, entryNum))
    (0 until inKey.length).foreach { i =>
      divider.io.in(i).bits.key := inKey(i)
      divider.io.in(i).bits.value := inValue(i)
      divider.io.in(i).valid := inValid(i)
      divider.io.inAgePtr(i) := inAgePtr(i)
    }
    divider.io.redirectAgePtr := redirectAgePtr
    divider.io.commitAgePtr := commitAgePtr

    divider.suggestName(s"specPathDivider_${tableName}_$siteName")

    class NewKeyType extends Bundle {
      val pl = chiselTypeOf(inKey.head)
      val isArch = Bool()
    }
    val keyOut = Wire(Vec(divider.io.out.length, new NewKeyType))
    val valueOut = Wire(Vec(divider.io.out.length, chiselTypeOf(inValue.head)))
    val enOut = Wire(Vec(divider.io.out.length, Bool()))
    (0 until divider.io.out.length).foreach { i =>
      keyOut(i).pl := divider.io.out(i).bits.key
      keyOut(i).isArch := divider.io.outIsArch(i)
      valueOut(i) := divider.io.out(i).bits.value
      enOut(i) := divider.io.out(i).valid
    }

    val chiselmap = ChiselMap.createTableBase(tableName, chiselTypeOf(keyOut), chiselTypeOf(valueOut), basicDB = basicDB)
    chiselmap.log(keyOut, valueOut, enOut, siteName, clock, reset)
  }
}