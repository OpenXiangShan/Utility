/**************************************************************************************
 * Copyright (c) 2025 Institute of Computing Technology, CAS
 * Copyright (c) 2025 University of Chinese Academy of Sciences
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

package utility.sram

import chisel3._
import chisel3.util._
import chisel3.experimental.hierarchy.core.Instance

class SramCtlBundle extends Bundle {
  val MCR   = UInt(2.W)
  val MCW   = UInt(2.W)
  val RTSEL = UInt(2.W)
  val WTSEL = UInt(2.W)
  val RCT   = UInt(2.W)
  val WCT   = UInt(2.W)
  val KP    = UInt(3.W)
}
