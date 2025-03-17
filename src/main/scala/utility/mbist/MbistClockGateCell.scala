/**************************************************************************************
 * Copyright (c) 2020 Institute of Computing Technology, CAS
 * Copyright (c) 2020 University of Chinese Academy of Sciences
 *
 * NutShell is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *             http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
 * FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/
package utility.mbist

import chisel3._
import utility.ClockGate
import utility.sram.SramBroadcastBundle

class CgDftBundle extends Bundle {
  val ram_mcp_hold = Input(Bool())
  val ram_aux_clk = Input(Bool())
  val ram_aux_ckbp = Input(Bool())
  val cgen = Input(Bool())
  def fromBroadcast(brc: SramBroadcastBundle): Unit = {
    ram_aux_clk := brc.mbist.ram_aux_clk
    ram_aux_ckbp := brc.mbist.ram_aux_ckbp
    ram_mcp_hold := brc.mbist.ram_mcp_hold
    cgen := brc.mbist.cgen
  }
}

class MbistClockGateCell(mcpCtl:Boolean) extends Module {
  val mbist = IO(new Bundle {
    val writeen = Input(Bool())
    val readen = Input(Bool())
    val req = Input(Bool())
  })
  val E = IO(Input(Bool()))
  val dft = IO(new CgDftBundle)
  val out_clock = IO(Output(Clock()))

  private val CG = Module(new ClockGate)
  CG.io.TE := dft.cgen
  CG.io.CK := clock

  if(mcpCtl) {
    CG.io.E := Mux(mbist.req, mbist.readen | mbist.writeen, E) && !dft.ram_mcp_hold
    out_clock := Mux(dft.ram_aux_ckbp, dft.ram_aux_clk.asClock, CG.io.Q)
  } else {
    CG.io.E := Mux(mbist.req, mbist.readen | mbist.writeen, E)
    out_clock := CG.io.Q
  }
}
