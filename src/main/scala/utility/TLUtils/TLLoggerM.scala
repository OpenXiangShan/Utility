/***************************************************************************************
* Copyright (c) 2020-2026 Institute of Computing Technology, Chinese Academy of Sciences
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
import chisel3.util.{ValidIO, log2Ceil}
import freechips.rocketchip.tilelink._

class TLLoggerMEntry(sourceBits: Int, dataBits: Int) extends Bundle {
  val source = UInt(sourceBits.W)
  val data = UInt(dataBits.W)
}

class TLLoggerM(name: String, edgeIn: TLEdgeOut, enable: Boolean, mDataBits: Int = 512) extends Module {
  private val numClients = edgeIn.client.endSourceId
  private val sourceBits = math.max(1, log2Ceil(numClients))
  private val addrBits = edgeIn.bundle.addressBits

  val io = IO(new Bundle {
    val a = Flipped(ValidIO(new TLBundleA(edgeIn.bundle)))
    val m = Flipped(ValidIO(new TLLoggerMEntry(sourceBits, mDataBits)))
  })

  if (enable) {
    val aDAddrs = Reg(Vec(numClients, UInt(addrBits.W)))
    when(io.a.valid) {
      aDAddrs(io.a.bits.source) := io.a.bits.address
    }

    val log = WireInit(0.U.asTypeOf(new TLLog))
    log.channel := 5.U // a-e: 0-4, m: 5
    log.opcode := 8.U
    log.param := 0.U
    log.source := io.m.bits.source
    log.sink := 0.U
    log.address := aDAddrs(io.m.bits.source)
    // TODO: logdata is 256b, mdata is 512b
    log.data := io.m.bits.data(mDataBits - 1, mDataBits - 256).asTypeOf(log.data)
    log.user := 0.U
    log.echo := 0.U

    TLLogger.table.log(log, io.m.valid, name, clock, reset)
  }
}
