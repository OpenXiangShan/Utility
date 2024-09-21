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

object BoolStopWatch {
  def apply(start: Bool, stop: Bool, startHighPriority: Boolean = false, bypass: Boolean = false) = {
    val r = RegInit(false.B)
    if (startHighPriority) {
      when (stop) { r := false.B }
      when (start) { r := true.B }
    }
    else {
      when (start) { r := true.B }
      when (stop) { r := false.B }
    }
    (if (bypass) (start || r) else r)
  }
}

object StartStopCounter {
  def apply(start: Bool, stop: Bool, init : BigInt, flush: Bool) = {
    val counter = RegInit(0.U(64.W))
    val working = RegInit(false.B)

    when (working) { counter := counter + 1.U }
    when (stop) {
      working := false.B
    }
    when (start) {
      counter := init.U
      working := true.B
    }
    counter
  }
}

