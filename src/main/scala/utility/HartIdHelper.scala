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

object HartIdHelper {
  private var hartIdNum = 1
  def setHartNum(num: Int): Unit = (hartIdNum = num)
  def getHartNum: Int = hartIdNum
}

private class HartIdDeclare extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val hartId = Input(UInt(64.W))
  })

  val verilog =
    """
      |module HartIdDeclare(
      |  input [63:0] hartId
      |);
      |
      |endmodule
      |""".stripMargin
  setInline("HartIdDeclare.v", verilog)
}

trait DeclareHartId {
  private val _hartIdDeclareModule = Module(new HartIdDeclare)
  val declaredHartId = Wire(UInt(64.W))
  _hartIdDeclareModule.io.hartId := declaredHartId
}
