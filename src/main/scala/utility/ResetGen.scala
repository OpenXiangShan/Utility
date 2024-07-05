/***************************************************************************************
 * Copyright (c) 2020-2022 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2022 Peng Cheng Laboratory
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
class DFTResetSignals extends Bundle{
  val lgc_rst_n = AsyncReset()
  val mode = Bool()
  val scan_mode = Bool()
}

class ResetGen(SYNC_NUM: Int = 2) extends Module {
  val o_reset = IO(Output(AsyncReset()))
  val dft = IO(Input(new DFTResetSignals()))
  val raw_reset = IO(Output(AsyncReset()))

  val lgc_rst = !dft.lgc_rst_n.asBool
  val real_reset = Mux(dft.mode, lgc_rst, reset.asBool).asAsyncReset

  withClockAndReset(clock, real_reset){
    val pipe_reset = RegInit(((1L << SYNC_NUM) - 1).U(SYNC_NUM.W))
    pipe_reset := Cat(pipe_reset(SYNC_NUM - 2, 0), 0.U(1.W))
    raw_reset := pipe_reset(SYNC_NUM - 1).asAsyncReset
  }

  // deassertion of the reset needs to be synchronized.
  o_reset := Mux(dft.scan_mode, lgc_rst, raw_reset.asBool).asAsyncReset
}

trait ResetNode

case class ModuleNode(mod: Module) extends ResetNode
case class CellNode(reset: Reset) extends ResetNode
case class ResetGenNode(children: Seq[ResetNode]) extends ResetNode

object ResetGen {
  def apply(SYNC_NUM: Int = 2, dft:Option[DFTResetSignals] = None): AsyncReset = {
    val resetSync = Module(new ResetGen(SYNC_NUM))
    if(dft.isDefined) {
      resetSync.dft := dft.get
    } else {
      resetSync.dft := 0.U.asTypeOf(new DFTResetSignals)
    }
    resetSync.o_reset
  }

  def apply(resetTree: ResetNode, reset: Reset, dft:Option[DFTResetSignals], sim: Boolean): Unit = {
    if(!sim) {
      resetTree match {
        case ModuleNode(mod) =>
          mod.reset := reset
        case CellNode(r) =>
          r := reset
        case ResetGenNode(children) =>
          val next_rst = Wire(Reset())
          withReset(reset){
            next_rst := ResetGen(2, dft)
          }
          children.foreach(child => apply(child, next_rst, dft, sim))
      }
    }
  }

  def apply(resetChain: Seq[Seq[Module]], reset: Reset,  dft:Option[DFTResetSignals], sim: Boolean): Seq[Reset] = {
    val resetReg = Wire(Vec(resetChain.length + 1, Reset()))
    resetReg.foreach(_ := reset)
    for ((resetLevel, i) <- resetChain.zipWithIndex) {
      if (!sim) {
        withReset(resetReg(i)) {
          resetReg(i + 1) := ResetGen(2, dft)
        }
      }
      resetLevel.foreach(_.reset := resetReg(i + 1))
    }
    resetReg.tail
  }

  def applyOneLevel(resetSigs: Seq[Reset], reset: Reset, sim: Boolean): DFTResetSignals = {
    val resetReg = Wire(Reset())
    val dft = Wire(new DFTResetSignals())
    resetReg := reset
    if (!sim) {
      withReset(reset) {
        val resetGen = Module(new ResetGen)
        resetReg := resetGen.o_reset
        resetGen.dft := dft
      }
    }
    resetSigs.foreach(_ := resetReg)
    dft
  }
}
