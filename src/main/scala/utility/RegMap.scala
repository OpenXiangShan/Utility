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

object RegMap {
  def Unwritable = null
  def apply(addr: Int, reg: UInt, wfn: UInt => UInt = (x => x)) = (addr, (reg, wfn))
  def generate(mapping: Map[Int, (UInt, UInt => UInt)], raddr: UInt, rdata: UInt,
    waddr: UInt, wen: Bool, wdata: UInt, wmask: UInt):Unit = {
    val chiselMapping = mapping.map { case (a, (r, w)) => (a.U, r, w) }
    rdata := LookupTree(raddr, chiselMapping.map { case (a, r, w) => (a, r) })
    chiselMapping.map { case (a, r, w) =>
      if (w != null) when (wen && waddr === a) { r := w(MaskData(r, wdata, wmask)) }
    }
  }
  def generate(mapping: Map[Int, (UInt, UInt => UInt)], addr: UInt, rdata: UInt,
    wen: Bool, wdata: UInt, wmask: UInt):Unit = generate(mapping, addr, rdata, addr, wen, wdata, wmask)
}

object MaskedRegMap { // TODO: add read mask
  def Unwritable = null
  def NoSideEffect: UInt => UInt = (x=>x)
  def WritableMask = Fill(64, true.B)
  def UnwritableMask = 0.U(64.W)
  def apply(addr: Int, reg: UInt,
            wmask: UInt = WritableMask, wfn: UInt => UInt = (x => x),
            rmask: UInt = WritableMask, rfn: UInt => UInt = x=>x
           ): (Int, (UInt, UInt, UInt => UInt, UInt, UInt => UInt)) = (addr, (reg, wmask, wfn, rmask, rfn))
  def generate(mapping: Map[Int, (UInt, UInt, UInt => UInt, UInt, UInt => UInt)], raddr: UInt, rdata: UInt,
    waddr: UInt, wen: Bool, wdata: UInt):Unit = {
    val chiselMapping = mapping.map { case (a, (r, wm, w, rm, rfn)) => (a.U, r, wm, w, rm, rfn) }
    rdata := LookupTree(raddr, chiselMapping.map { case (a, r, _, _, rm, rfn) => (a, rfn(r & rm)) })
    val wdata_reg = RegEnable(wdata, wen)
    chiselMapping.foreach { case (a, r, wm, w, _, _) =>
      if (w != null && wm != UnwritableMask) {
        // Warning: this RegMap adds a RegNext for write to reduce fanout
        // the w must be pure function without side effects
        val wen_reg = GatedValidRegNext(wen && waddr === a)
        when (wen_reg) { r := w(MaskData(r, wdata_reg, wm)) }
      }
    }
  }
  def isIllegalAddr(mapping: Map[Int, (UInt, UInt, UInt => UInt, UInt, UInt => UInt)], addr: UInt):Bool = {
    val illegalAddr = Wire(Bool())
    illegalAddr := LookupTreeDefault(addr, true.B, mapping.toSeq.sortBy(_._1).map { case (a, _) => (a.U, false.B) })
    illegalAddr
  }
  def generate(mapping: Map[Int, (UInt, UInt, UInt => UInt, UInt, UInt => UInt)], addr: UInt, rdata: UInt,
    wen: Bool, wdata: UInt):Unit = generate(mapping, addr, rdata, addr, wen, wdata)
}


// Case class for register entries - used with ConditionalRegMap
case class RegMapEntry(
  reg: UInt, 
  wmask: UInt = Fill(64, true.B),      // Write mask - default allows all writes
  wfn: UInt => UInt = (x => x),        // Write function
  rmask: UInt = Fill(64, true.B),      // Read mask - default allows all reads  
  rfn: UInt => UInt = (x => x)         // Read function
)

// Conditional register map entry with mask support
case class ConditionalRegMapEntry(
  condition: Bool, 
  reg: UInt, 
  wmask: UInt = Fill(64, true.B),      // Write mask - default allows all writes
  wfn: UInt => UInt = (x => x),        // Write function
  rmask: UInt = Fill(64, true.B),      // Read mask - default allows all reads  
  rfn: UInt => UInt = (x => x)         // Read function
)

// Extended RegMap with Conditional support
object ConditionalRegMap {
  def Unwritable = null
  def NoSideEffect: UInt => UInt = (x => x)
  def WritableMask = Fill(64, true.B)
  def UnwritableMask = 0.U(64.W)
  
  // Type alias for the mapping value - now unified as Seq[ConditionalRegMapEntry]
  type MappingValue = Seq[ConditionalRegMapEntry]
  
  // Implicit conversions to make usage seamless without Left/Right
  implicit def regMapEntryToSeq(entry: RegMapEntry): Seq[ConditionalRegMapEntry] = 
    Seq(ConditionalRegMapEntry(true.B, entry.reg, wmask = entry.wmask, wfn = entry.wfn, rmask = entry.rmask, rfn = entry.rfn))
  
  implicit def regToSeq(reg: UInt): Seq[ConditionalRegMapEntry] = 
    Seq(ConditionalRegMapEntry(true.B, reg))
    
  // Generate the register mapping logic with MaskedRegMap features
  def generate(mapping: Map[Int, MappingValue], raddr: UInt, rdata: UInt, waddr: UInt, wen: Bool, wdata: UInt, wmask: UInt): Unit = {
    
    val readCases = scala.collection.mutable.ArrayBuffer[(UInt, UInt)]()
    
    mapping.foreach { case (addr, conditionalRegs) =>
      val addrU = addr.U
      
      require(conditionalRegs.nonEmpty, "ConditionalRegMap: At least one conditional register must be provided")
      
      val conditions = conditionalRegs.map(_.condition)
      val registers = conditionalRegs.map(_.reg)
      val readMasks = conditionalRegs.map(_.rmask)
      val readFunctions = conditionalRegs.map(_.rfn)
      
      // Use Mux1H for OneHot selection of registers, masks, and functions
      val selectedReg = if (conditionalRegs.length == 1) {
        val condReg = conditionalRegs.head
        condReg.rfn(condReg.reg & condReg.rmask)
      } else {
        val readValues = conditionalRegs.map(condReg => condReg.rfn(condReg.reg & condReg.rmask))
        Mux1H(conditions, readValues)
      }
      readCases += ((addrU, selectedReg))
      
      // Write logic for each conditional register with mask support
      conditionalRegs.foreach { condReg =>
        if (condReg.wfn != null && condReg.wmask != UnwritableMask) {
          // Use RegNext approach similar to MaskedRegMap for fanout reduction
          val wdata_reg = RegEnable(wdata, wen)
          val wen_reg = GatedValidRegNext(wen && waddr === addrU && condReg.condition)
          
          when (wen_reg) {
            // Apply write mask and write function
            val maskedData = MaskData(condReg.reg, wdata_reg, condReg.wmask & wmask)
            condReg.reg := condReg.wfn(maskedData)
          }
        }
      }
    }
    
    // Generate read logic using LookupTree
    rdata := LookupTree(raddr, readCases.toSeq)
  }
  
  // Simplified version where read and write use the same address
  def generate(mapping: Map[Int, MappingValue], addr: UInt, rdata: UInt, wen: Bool, wdata: UInt): Unit = {
    generate(mapping, addr, rdata, addr, wen, wdata, Fill(64, true.B))
  }

  // Check if an address is illegal (similar to MaskedRegMap)
  def isIllegalAddr(mapping: Map[Int, MappingValue], addr: UInt): Bool = {
    val illegalAddr = Wire(Bool())
    illegalAddr := LookupTreeDefault(addr, true.B, mapping.toSeq.sortBy(_._1).map { case (a, _) => (a.U, false.B) })
    illegalAddr
  }

  /* Usage Example:
   * 
   * import utility._
   * import utility.ConditionalRegMap._
   *
   * // Create mapping 
   * val mapping: Map[Int, ConditionalRegMap.MappingValue] = Map(
   *   0x00 -> RegMapEntry(regA, wfn = null),
   *   0x04 -> regB,
   *   0x08 -> Seq(
   *     // Note: condition (cond1 and cond2 here) should be one-hot
   *     ConditionalRegMapEntry(cond1, regC, 
   *       wmask = 0xFF.U,
   *       wfn = (x => x + 1.U),
   *       rmask = Fill(32, true.B),
   *       rfn = (x => x - 1.U)
   *     ),
   *     ConditionalRegMapEntry(cond2, regD,)
   *   )
   * )
   * 
   * // Generate the register mapping
   * ConditionalRegMap.generate(mapping, io.addr, io.rdata, io.wen, io.wdata, io.wmask)
   */
}