/***************************************************************************************
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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
import chisel3.util.HasBlackBoxInline
import chisel3.util.experimental.BoringUtils

import scala.collection.mutable

class ClockGate extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val TE = Input(Bool())
    val E  = Input(Bool())
    val CK = Input(Clock())
    val Q  = Output(Clock())
  })

  val sverilog =
    """
      |module ClockGate (
      |  input  wire TE,
      |  input  wire E,
      |  input  wire CK,
      |  output wire Q
      |);
      |  reg EN;
      |  always_latch begin
      |    if(!CK) EN = TE | E;
      |  end
      |  assign Q = CK & EN;
      |endmodule
      |
      |""".stripMargin
  setInline("ClockGate.sv", sverilog)
}

class ClockGateTeBundle extends Bundle {
  val cgen = Input(Bool())
}

object ClockGate {
  private val teQueue = new mutable.Queue[ClockGateTeBundle]
  def apply(TE: Bool, E: Bool, CK: Clock) : Clock = {
    val clock_gate = Module(new ClockGate).io
    clock_gate.TE := TE
    clock_gate.E  := E
    clock_gate.CK := CK
    clock_gate.Q
  }

  def genTeSink: ClockGateTeBundle = {
    val te = Wire(new ClockGateTeBundle)
    te := 0.U.asTypeOf(te)
    dontTouch(te)
    teQueue.enqueue(te)
    te
  }

  def genTeSrc: ClockGateTeBundle = {
    val res = Wire(new ClockGateTeBundle)
    teQueue.toSeq.foreach(bd => {
      BoringUtils.bore(bd) := res
    })
    teQueue.clear()
    res
  }
}
