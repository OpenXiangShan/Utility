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
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4.AXI4Parameters._
import freechips.rocketchip.diplomacy._

class AXI4Error(
  addressRange: Seq[AddressSet],
  buffer: Boolean = true
)(implicit p: Parameters) extends LazyModule {
  val node = AXI4SlaveNode(Seq(
    AXI4SlavePortParameters(
      slaves = Seq(
        AXI4SlaveParameters(
          address = addressRange,
          regionType = RegionType.VOLATILE,
          executable = false,
          supportsRead = TransferSizes(1, 8),
          supportsWrite = TransferSizes(1, 8),
          interleavedId = Some(0)
        )
      ),
      beatBytes = 8
    )
  ))

  lazy val module = new Impl(this)
  // TODO: consider burst transfer
  class Impl(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val (in, edge) = node.in.head

    // read
    val ar = if (buffer) { Queue(in.ar, 1) } else in.ar
    val r = Wire(chiselTypeOf(in.r))

    ar.ready := r.ready
    r.valid := ar.valid

    r.bits.id := ar.bits.id
    r.bits.data := 0.U
    r.bits.resp := RESP_DECERR
    r.bits.last := true.B
    in.r <> r

    // write
    val aw = if (buffer) { Queue(in.aw, 1) } else in.aw
    val w = if (buffer) { Queue(in.w, 1) } else in.w
    val b = Wire(chiselTypeOf(in.b))

    val idle_aw = RegInit(true.B)
    val idle_w = RegInit(true.B)

    when (b.fire) { idle_aw := true.B }
    .elsewhen (aw.valid) { idle_aw := false.B }

    when (b.fire) { idle_w := true.B }
    .elsewhen (w.valid && w.bits.last) { idle_w := false.B }

    aw.ready := b.fire
    w.ready := b.fire || !w.bits.last
    b.valid := !idle_aw && !idle_w

    b.bits.id := aw.bits.id
    b.bits.resp := RESP_DECERR
    in.b <> b
  }
}