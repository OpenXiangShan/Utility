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

trait HasCriticalErrors { this: RawModule =>
  val criticalErrors: Seq[(String, Bool)]

  lazy val io_error: Vec[Bool] = IO(Output(Vec(criticalErrors.length, Bool())))
  def generateCriticalErrors(noRegNext: Option[Seq[Int]] = None): Unit = {
    for (((out, (name, error)), i) <- io_error.zip(criticalErrors).zipWithIndex) {
      require(!name.contains("/"))
      out := RegNext(RegNext(error))
      if (noRegNext.isDefined && noRegNext.get.contains(i)) {
        out := error
      }
    }
  }

  def getCriticalErrors: Seq[(String, Bool)] = {
    criticalErrors.map(_._1).zip(io_error).map(x => (x._1, x._2))
  }

}