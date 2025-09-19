/***************************************************************************************
* Copyright (c) 2024-2025 Beijing Institute of Open Source Chip
*
* Utility is licensed under Mulan PSL v2.
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
import chisel3.experimental.StringParam
import chisel3.util._
import freechips.rocketchip.util.DataToAugmentedData
import java.lang
import freechips.rocketchip.amba.ahb.AHBImpMaster.bundle

trait HasDPICUtils extends BlackBox with HasBlackBoxInline {
  var moduleName: String = ""
  def init(args: Bundle, negedge: Boolean = false, comb_output: Boolean = false) = {
    val field = args.elements.map(t => {
      val name = t._1
      val tpes = t._2.getClass.getMethods.map(x => x.getName()).toList
      val tpe = classOf[UInt].getMethod("specifiedDirection")
      val is_input = tpe.invoke(t._2) == SpecifiedDirection.Input
      (name, is_input)
    }).toList.reverse.drop(3)

    val ports_input = field.filter(_._2).map(_._1)
    val ports_output = field.filter(!_._2).map(_._1)
    val has_out = ports_output.size != 0
    val has_in = ports_input.size != 0
    assert(ports_output.size <= 1)
    assert(if (has_out) true else !comb_output)
    val port_output = if (has_out) ports_output.head else ""
    val out_reg = port_output + "_reg"
    val assign = if (comb_output) "=" else "<="
    val clock = if (comb_output) "*" else ((if (negedge) "negedge" else "posedge") + " clock")

    if (ports_input.contains(List("clock", "reset", "en"))) {
      throw new Exception
    }

    val className = this.getClass().getSimpleName()
    moduleName = className + "_DPIC_Helper"
    val dpicFunc = lang.Character.toLowerCase(className.charAt(0)) + className.substring(1)
    val verilog =
      s"""
         |import "DPI-C" function ${if (has_out) "longint unsigned" else "void"} $dpicFunc
         |(
         |${if (has_in)  ports_input.map(x => "input longint unsigned " + x).mkString("", ",\n  ", "") else ""}
         |);
         |
         |module $moduleName(
         |input clock,
         |input reset,
         |input en
         |${if (has_out) ",\noutput [63:0]" + port_output else ""}
         |${if (has_in) ports_input.map(x => "input [63:0] " + x).mkString(",", ",\n", "") else ""}
         |);
         |  ${if (has_out) "reg [63: 0] " + out_reg + ";" else ""}
         |  always@($clock) begin
         |    if (reset) begin
         |      ${if (has_out) out_reg + assign + "0" else ""};
         |    end
         |    else if(en) begin
         |      ${if (has_out) out_reg + assign else "  "}$dpicFunc(${ports_input.mkString("", ", ", "")});
         |    end
         |    else begin
         |      ${if (comb_output) out_reg + assign + "0" else ""};
         |    end
         |  end
         |
         |  ${if (has_out) "assign " + port_output + " = " + out_reg + ";" else ""}
         |endmodule
         |""".stripMargin

    setInline(s"$moduleName.sv", verilog)
  }

  override def desiredName: String = moduleName
}

class GlobalSimNegedge extends HasDPICUtils {
  // only can be instantiated once
  val io = IO(new Bundle() {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val en = Input(Bool())
  })
  init(io, true)
}

class CreateInstMeta extends HasDPICUtils {
  val io = IO(new Bundle() {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val en = Input(Bool())
    val sn = Output(UInt(64.W))
    val order_id = Input(UInt(64.W))
    val pc = Input(UInt(64.W))
    val instcode = Input(UInt(64.W))
  })
  init(io, false, true)
}

class UpdateInstPos extends HasDPICUtils {
  val io = IO(new Bundle() {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val en = Input(Bool())
    val sn = Input(UInt(64.W))
    val pos = Input(UInt(64.W))
  })
  init(io)
}

class UpdateInstMeta extends HasDPICUtils {
  val io = IO(new Bundle() {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val en = Input(Bool())
    val sn = Input(UInt(64.W))
    val meta = Input(UInt(64.W))
    val data = Input(UInt(64.W))
  })
  init(io)
}

class CommitInstMeta extends HasDPICUtils {
  val io = IO(new Bundle() {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val en = Input(Bool())
    val order_id = Input(UInt(64.W))
    val sn_first = Input(UInt(64.W))
    val block_num_insts = Input(UInt(64.W))
  })
  init(io)
}

object TaggedTrace {
  object InstPos extends Enumeration {
    val AtFetch = Value("AtFetch")
    val AtDecode = Value("AtDecode")
    val AtRename = Value("AtRename")
    val AtDispQue = Value("AtDispQue")
    val AtIssueQue = Value("AtIssueQue")
    val AtIssueArb = Value("AtIssueArb")
    val AtIssueReadReg = Value("AtIssueReadReg")
    val AtFU = Value("AtFU")
    val AtBypassVal = Value("AtBypassVal")
    val AtWriteVal = Value("AtWriteVal")
    val AtCommit = Value("AtCommit")
  }

  object InstRecord extends Enumeration {
    val DisAsm = Value("DisAsm")
    val PC = Value("PC")
  }

  var enableCCT = false
  
  def init(enable: Boolean) {
    enableCCT = enable
  }

  def tick(clock: Clock, reset: Reset) {
    if (enableCCT) {
      val m = Module(new GlobalSimNegedge)
      m.io.clock := clock
      m.io.reset := reset
      m.io.en := true.B
    }
  }

  def createInstMetaAtFetch(order_id: UInt, pc: UInt, code: UInt, en: Bool, clock: Clock, reset: Reset): UInt = {
    if (enableCCT) {
      val m = Module(new CreateInstMeta)
      m.io.clock := clock
      m.io.reset := reset
      m.io.en := en
      m.io.order_id := order_id
      m.io.pc := pc
      m.io.instcode := code
      return m.io.sn
    }
    return 0.U
  }

  def updateInstPos(sn: UInt, pos: UInt, en: Bool, clock: Clock, reset: Reset) {
    if (enableCCT) {
      val m = Module(new UpdateInstPos)
      m.io.clock := clock
      m.io.reset := reset
      m.io.en := en
      m.io.sn := sn
      m.io.pos := pos
    }
  }

  def updateInstMeta(sn: UInt, meta: UInt, data: UInt, en: Bool, clock: Clock, reset: Reset) {
    if (enableCCT) {
      val m = Module(new UpdateInstMeta)
      m.io.clock := clock
      m.io.reset := reset
      m.io.en := en
      m.io.sn := sn
      m.io.meta := meta
      m.io.data := data
    }
  }

  def commitInstMeta(order_id: UInt, sn: UInt, block_size: UInt, en: Bool, clock: Clock, reset: Reset) {
    if (enableCCT) {
      val m = Module(new CommitInstMeta)
      m.io.clock := clock
      m.io.reset := reset
      m.io.en := en
      m.io.order_id := order_id
      m.io.sn_first := sn
      m.io.block_num_insts := block_size
    }
  }

  def getCHeader: String = {
    s"""
       |#ifndef __CHISEL_PERFCCT_H__
       |#define __CHISEL_PERFCCT_H__
       |
       |#include <cstdio>
       |#include <cstring>
       |#include <cstdlib>
       |#include <cassert>
       |#include <cstdint>
       |#include <cerrno>
       |#include <unistd.h>
       |#include <sqlite3.h>
       |#include <vector>
       |
       |enum InstPos {
       |  ${InstPos.values.mkString("", ",\n  ", "")}
       |};
       |
       |
       |struct InstMeta
       |{
       |  uint64_t sn;
       |  uint64_t pc;
       |  uint64_t instcode;
       |  std::vector<uint64_t> posTick;
       |
       |  void reset(uint64_t sn, uint64_t pc, uint64_t instcode) {
       |    this->sn = sn;
       |    this->pc = pc;
       |    this->instcode = instcode;
       |    posTick.clear();
       |    posTick.resize(${InstPos.AtCommit.id} + 1, 0);
       |  }
       |};
       |
       |// export to xspdb
       |extern std::vector<InstMeta*> lastCommittedInsts;
       |
       |#endif
       """.stripMargin
  } 
 
  def getCpp: String = {
    s"""
       |// performanceCounter commitTrace
       |
       |#include "perfCCT.h"
       |#include "chisel_db.h"
       |#include <string>
       |#include <sstream>
       |#include <mutex>
       |
       |extern sqlite3 *mem_db;
       |extern char *zErrMsg;
       |extern int rc;
       |int callback_temp(void *NotUsed, int argc, char **argv, char **azColName) { return 0; }
       |
       |// must define these
       |extern std::string riscv_disasm(uint64_t code, uint64_t pc);
       |
       |bool enable_dump_lifetime = false;
       |uint64_t global_tick_acc = 0;
       |
       |std::vector<InstMeta*> lastCommittedInsts;
       |
       |class PerfCCT
       |{
       |  const int MaxMetas = 3000;
       |  uint64_t cur_tick = 0;
       |  uint64_t sn_acc = 10;
       |  uint64_t last_max_sn = sn_acc;
       |
       |  std::vector<InstMeta> metas;
       |  InstMeta invalidMeta;
       |  std::vector<std::vector<InstMeta*>> commitOrderQ;
       |  std::string sql_insert_cmd;
       |
       |  std::mutex createLock;
       |  std::mutex commitLock;
       |  std::stringstream ss;
       |
       |  InstMeta* getMeta(uint64_t sn) {
       |    if (sn == 0) [[unlikely]] return &invalidMeta;
       |    return &metas[sn%MaxMetas];
       |  }
       |
       |public:
       |  PerfCCT() {
       |#if ${enableCCT.toString()}
       |    metas.resize(MaxMetas);
       |    invalidMeta.reset(0, 0, 0);
       |
       |    ss << "INSERT INTO LifeTimeCommitTrace(";
       |    ss << ${InstPos.values.mkString("\"", ",", ",")}${InstRecord.values.mkString("", ",", "\"")};
       |    ss << ") VALUES (";
       |    sql_insert_cmd = ss.str();
       |    ss.str(std::string());
       |
       |    const char* createTable=
       |    "CREATE TABLE LifeTimeCommitTrace( \\
       |    ID INTEGER PRIMARY KEY AUTOINCREMENT, \\
       |    ${InstPos.values.mkString("", " INT NOT NULL, \\\n      ", " INT NOT NULL, \\")}
       |    DisAsm INT NOT NULL, \\
       |    PC INT NOT NULL \\
       |    );";
       |
       |    rc = sqlite3_exec(mem_db, createTable, callback_temp, 0, &zErrMsg);
       |    if (rc != SQLITE_OK) {
       |      printf("PerfCCT SQL error: %s\\n", zErrMsg);
       |      exit(1);
       |    }
       |
       |#endif
       |  }
       |
       |  void tick() {
       |#if ${enableCCT.toString()}
       |    if (!enable_dump_lifetime) [[likely]] return;
       |    // negedge trigger
       |    if (cur_tick != global_tick_acc) {
       |		  cur_tick = global_tick_acc;
       |      // update last_max_sn
       |      last_max_sn = sn_acc + 1;
       |
       |      lastCommittedInsts.clear();
       |      // dump last commmitted insts
       |      for (auto& it : commitOrderQ) {
       |        for (auto meta:it) {
       |          if (meta == &invalidMeta) [[unlikely]] continue;
       |          ss << sql_insert_cmd;
       |          ss << meta->posTick[0];
       |          for (auto it = meta->posTick.begin() + 1; it != meta->posTick.end(); it++) {
       |              ss << "," << *it;
       |          }
       |          ss << "," << meta->instcode;
       |          // pc is unsigned, but sqlite3 only supports signed integer [-2^63, 2^63-1]
       |          // if real pc > 2^63-1, it will be stored as negative number 
       |          // (negtive pc = real pc - 2^64)
       |          // when read a negtive pc, real pc = negtive pc + 2^64
       |          ss << "," << int64_t(meta->pc); 
       |          ss << ");";
       |
       |          rc = sqlite3_exec(mem_db, ss.str().c_str(), callback_temp, 0, &zErrMsg);
       |          if (rc != SQLITE_OK) {
       |            printf("commitMeta SQL error: %s\\n", zErrMsg);
       |            exit(1);
       |          }
       |          ss.str(std::string());
       |          lastCommittedInsts.push_back(meta);
       |        }
       |        it.clear();
       |      }
       |    }
       |#endif
       |  }
       |
       |  uint64_t createInstMeta(uint64_t order_id, uint64_t pc, uint64_t instcode) {
       |#if ${enableCCT.toString()}
       |    if (!enable_dump_lifetime) [[likely]] return 0;
       |    createLock.lock();
       |
       |    uint64_t sn = last_max_sn + order_id;
       |    sn_acc = std::max(sn_acc, sn);
       |    auto old = getMeta(sn);
       |    old->reset(sn, pc, instcode);
       |    old->posTick.at(AtFetch) = global_tick_acc;
       |
       |    createLock.unlock();
       |    return sn;
       |#endif
       |    return 0;
       |  }
       |
       |  void updateInstPos(uint64_t sn, const InstPos pos) {
       |#if ${enableCCT.toString()}
       |    if (!enable_dump_lifetime) [[likely]] return;
       |
       |    auto meta = getMeta(sn);
       |    meta->posTick.at(pos) = global_tick_acc;
       |#endif
       |  }
       |
       |  // interface example
       |  void updateInstMeta(uint64_t, uint64_t, uint64_t) {
       |#if ${enableCCT.toString()}
       |    if (!enable_dump_lifetime) [[likely]] return;
       |    // do somthing
       |#endif
       |  }
       |
       |  void commitMeta(uint64_t order_id, uint64_t sn, uint64_t block_size) {
       |#if ${enableCCT.toString()}
       |    if (!enable_dump_lifetime) [[likely]] return;
       |    commitLock.lock();
       |
       |    // dynamic resize
       |    if (order_id >= commitOrderQ.size()) {
       |      commitOrderQ.resize(order_id + 1);
       |    }
       |
       |    for (int i=0;i<block_size;i++) {
       |      auto meta = getMeta(sn + i);
       |      meta->posTick.at(AtCommit) = global_tick_acc;
       |      commitOrderQ[order_id].push_back(meta);
       |    }
       |
       |    commitLock.unlock();
       |#endif
       |  }
       |}*perfCCT;
       |
       |void init_perfcct(char *((*select_db_list)[256]), int select_db_num) {
       |  perfCCT = new PerfCCT();
       |
       |  const char *table_name = "lifetime";
       |  for (int idx = 0; idx < select_db_num; idx++) {
       |    char *str_p = (*select_db_list)[idx];
       |    int s_idx = 0;
       |    bool match = true;
       |    for (; (str_p[s_idx] != '\\0') && (table_name[s_idx] != '\\0'); s_idx ++) {
       |      if (str_p[s_idx] != table_name[s_idx]) {
       |        match = false;
       |        break;
       |      }
       |    }
       |    if (!match || (str_p[s_idx] != '\\0')) continue;
       |
       |    enable_dump_lifetime = true;
       |    break;
       |  }
       |}
       |
       |extern "C" {
       |  // dpic function name must same as HasDPICUtils subclass's name
       |  // here add your new interface
       |
       |  void globalSimNegedge() {
       |    global_tick_acc++;
       |    perfCCT->tick();
       |  }
       |
       |  uint64_t createInstMeta(uint64_t order_id, uint64_t pc, uint64_t instcode) {
       |    return perfCCT->createInstMeta(order_id, pc, instcode);
       |  }
       |
       |  void updateInstPos(uint64_t sn, const InstPos pos) {
       |    perfCCT->updateInstPos(sn, pos);
       |  }
       |
       |  void updateInstMeta(uint64_t sn, uint64_t meta, uint64_t data) {
       |    perfCCT->updateInstMeta(sn, meta, data);
       |  }
       |
       |  void commitInstMeta(uint64_t order_id, uint64_t sn, uint64_t block_size) {
       |    perfCCT->commitMeta(order_id, sn, block_size);
       |  }
       |}
       """.stripMargin
  }

  def addToFileRegisters {
    FileRegisters.add("perfCCT.h", getCHeader)
    FileRegisters.add("perfCCT.cpp", getCpp)
  }
}
