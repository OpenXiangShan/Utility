/***************************************************************************************
* Copyright (c) 2025 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2025 Institute of Computing Technology, Chinese Academy of Sciences
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
import freechips.rocketchip.util._

object SelectByFn {

  class SelectByFn[T <: Data, SelectT <: Data](gen: T, selectGen: SelectT, numIn: Int, fn: (SelectT, SelectT) => Bool) extends Module {
    val io = IO(new Bundle() {
      val in = Flipped(Vec(numIn, ValidIO(chiselTypeOf(gen))))
      val sel = Input(Vec(numIn, chiselTypeOf(selectGen)))
      val chosen = ValidIO(chiselTypeOf(gen))
    })
    def treeSelect(ins: Seq[(Bool, (T, SelectT))]): Seq[(Bool, (T, SelectT))] = {
      ins.length match {
        case 0 | 1 => ins
        case 2     =>
          val (left, right) = (ins.head, ins.last)
          val chosen = MuxT(left._1 && right._1,
                        MuxT(fn(left._2._2, right._2._2), left._2, right._2),
                          MuxT(left._1 && !right._1, left._2, right._2))
          Seq((left._1 || right._1, chosen))
        case _      =>
          val left = treeSelect(ins.take(ins.length/2))
          val right = treeSelect(ins.drop(ins.length/2))
          treeSelect(left ++ right)
      }
    }

    val chosen = treeSelect(io.in.zip(io.sel).map {
      case (elem, sel) => (elem.valid, (elem.bits, sel))
    })
    io.chosen.valid := chosen.head._1
    io.chosen.bits  := chosen.head._2._1
  }

  /**
   * `SelectByFn` is a module designed to select the element from an
   * input sequence.
   *
   * @param ins The input elements.
   * @param sels The selector of the selection criterions.
   * @param fn A comparison function to determine the element based on the selection criterion.
   */

  def apply[T <: Data, SelectT <: Data](
    ins:  Seq[ValidIO[T]],
    sels: Seq[SelectT],
    fn:   (SelectT, SelectT) => Bool,
    moduleName: Option[String] = None
  ): ValidIO[T] = {
    require(ins.length == sels.length, "The number of elements and selectors should be the same!")
    val mod = Module(new SelectByFn(ins.head.bits, sels.head, ins.length, fn)).suggestName(moduleName.getOrElse("SelectByFn"))
    mod.io.in <> ins
    mod.io.sel <> sels
    mod.io.chosen
  }
}
