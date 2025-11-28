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

class InstSeqNum extends Bundle {
  val seqNum = UInt(56.W)
  val uopIdx = UInt(8.W)
}

object InstSeqNum {
  def apply() = new InstSeqNum
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
    val uopIdx = Input(UInt(64.W))
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
    val uopIdx = Input(UInt(64.W))
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

  object InstDetail extends Enumeration {
    val VAddress = Value("VAddress")
    val PAddress = Value("PAddress")
    val LastReplay = Value("LastReplay")
    val ReplayStr = Value("ReplayStr")
  }

  object ReplayReason extends Enumeration {
    val CacheMiss = Value("C")
    val TLBMiss = Value("T")
    val BankConflict = Value("B")
    val Nuke = Value("N")
    val DcacheStall = Value("S") // mshr full...
    val RARReplay = Value("R")
    val RAWReplay = Value("W")
    val STDForwardFail = Value("F")
    val OtherReplay = Value("O")
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

  def updateInstPos(seq: InstSeqNum, pos: UInt, en: Bool, clock: Clock, reset: Reset) {
    if (enableCCT) {
      val m = Module(new UpdateInstPos)
      m.io.clock := clock
      m.io.reset := reset
      m.io.en := en
      m.io.sn := seq.seqNum
      m.io.uopIdx := seq.uopIdx
      m.io.pos := pos
    }
  }

  def updateInstPos(sn: UInt, pos: UInt, en: Bool, clock: Clock, reset: Reset) {
    val seq = InstSeqNum()
    seq.seqNum := sn
    seq.uopIdx := 0.U
    updateInstPos(seq, pos, en, clock, reset)
  }

  def updateInstMeta(seq: InstSeqNum, meta: UInt, data: UInt, en: Bool, clock: Clock, reset: Reset) {
    if (enableCCT) {
      val m = Module(new UpdateInstMeta)
      m.io.clock := clock
      m.io.reset := reset
      m.io.en := en
      m.io.sn := seq.seqNum
      m.io.uopIdx := seq.uopIdx
      m.io.meta := meta
      m.io.data := data
    }
  }

  def updateInstMeta(sn: UInt, meta: UInt, data: UInt, en: Bool, clock: Clock, reset: Reset) {
    val seq = InstSeqNum()
    seq.seqNum := sn
    seq.uopIdx := 0.U
    updateInstMeta(seq, meta, data, en, clock, reset)
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
       |#include <mutex>
       |#include <map>
       |#include <sstream>
       |
       |enum InstPos {
       |  ${InstPos.values.mkString("", ",\n  ", "")}
       |};
       |
       |enum InstDetail {
       |  ${InstDetail.values.mkString("", ",\n  ", "")}
       |};
       |
       |
       |static char ReplayReasonStr[] = {
       |   ${ReplayReason.values.mkString("\'", "\',\n \'", "\'")}        
       |};
       |
       |struct InstMeta
       |{
       |  uint64_t sn;
       |  uint64_t pc;
       |  uint64_t instcode;
       |  std::vector< std::vector<uint64_t> > uopTick;
       |  std::mutex uopTickLock;
       |  // load meta not support split inst
       |  bool isload;
       |  uint64_t vaddr;
       |  uint64_t paddr;
       |  uint64_t lastReplay;
       |  std::stringstream replayStr;
       |
       |  void reset(uint64_t sn, uint64_t pc, uint64_t instcode) {
       |    std::lock_guard<std::mutex> guard(uopTickLock);
       |    this->sn = sn;
       |    this->pc = pc;
       |    this->instcode = instcode;
       |    uopTick.clear();
       |
       |    isload = false;
       |    vaddr = 0;
       |    paddr = 0;
       |    lastReplay = 0;
       |    replayStr.str(std::string());
       |  }
       |
       |  void updateUopTick(uint64_t uopIdx, const InstPos pos, uint64_t tick) {
       |    std::lock_guard<std::mutex> guard(uopTickLock);
       |    if (uopTick.size() <= uopIdx) {
       |      size_t old_size = uopTick.size();
       |      for (size_t i = old_size; i <= uopIdx; i++) {
       |        uopTick.push_back(std::vector<uint64_t>());
       |        uopTick.back().resize(AtCommit + 1, 0);
       |        // uop shares same fetch and decode time
       |        if (uopIdx > 0) {
       |          uopTick.back().at(AtFetch) = uopTick[0].at(AtFetch);
       |          uopTick.back().at(AtDecode) = uopTick[0].at(AtDecode);
       |        }
       |      }
       |    }
       |    if (pos == AtCommit) {
       |      // each uop commits at the same time
       |      for (auto& it : uopTick)
       |        it.at(AtCommit) = tick;
       |    } else {
       |      if (!uopTick[uopIdx].at(pos))
       |        uopTick[uopIdx].at(pos) = tick;
       |    }
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
       |  uint64_t cur_tick = 0;
       |  uint64_t sn_acc = 10;
       |  uint64_t last_max_sn = sn_acc;
       |  uint64_t commitSN = 0;
       |  uint64_t last_commitSN = 0;
       |
       |  std::map <uint64_t, InstMeta> metas;
       |  // never replace with unordered_map, we need ordered RB-Tree
       |  std::string sql_insert_cmd, ld_insert_cmd;
       |
       |  std::mutex commitLock; // guard commitSN
       |  std::mutex metaLock;   // guard metas
       |  std::stringstream ss;
       |
       |  uint64_t id = 0;
       |
       |  std::map<uint64_t, InstMeta>::iterator getMeta(uint64_t sn) {
       |    if (sn == 0) [[unlikely]] return metas.end();
       |    else {
       |      std::lock_guard<std::mutex> guardMeta(metaLock);
       |      metas[sn];
       |      return metas.find(sn);
       |    }
       |  }
       |
       |public:
       |  PerfCCT() {
       |#if ${enableCCT.toString()}
       |    ss << "INSERT INTO LifeTimeCommitTrace(SN,UopIdx,";
       |    ss << ${InstPos.values.mkString("\"", ",", ",")}${InstRecord.values.mkString("", ",", "\"")};
       |    ss << ") VALUES (";
       |    sql_insert_cmd = ss.str();
       |    ss.str(std::string());
       |
       |    ld_insert_cmd = "insert into LoadLifeTimeCommitTrace(ID, VAddress, PAddress, LastReplay, ReplayStr) Values (";
       |
       |    const char* createTable=
       |    "CREATE TABLE LifeTimeCommitTrace( \\
       |    ID INTEGER PRIMARY KEY AUTOINCREMENT, \\
       |    SN INTEGER NOT NULL, \\
       |    UopIdx INT NOT NULL, \\
       |    ${InstPos.values.mkString("", " bigint unsigned NOT NULL, \\\n    ", " bigint unsigned NOT NULL, \\")}
       |    DisAsm int unsigned NOT NULL, \\
       |    PC bigint unsigned NOT NULL \\
       |    ); \\
       |    CREATE TABLE LoadLifeTimeCommitTrace( \\
       |      ID int unsigned PRIMARY KEY, \\
       |      VAddress bigint unsigned not null, \\
       |      PAddress bigint unsigned not null, \\
       |      LastReplay bigint unsigned not null, \\
       |      ReplayStr char(10) not null, \\
       |      constraint fk_id \\
       |        foreign key (ID) references LifeTimeCommitTrace(ID) \\
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
       |  void tick() { // negedge trigger
       |#if ${enableCCT.toString()}
       |    if (!enable_dump_lifetime) [[likely]] return;
       |    if (cur_tick != global_tick_acc) {
       |      cur_tick = global_tick_acc;
       |      // update last_max_sn
       |      last_max_sn = sn_acc + 1;
       |
       |      std::lock_guard<std::mutex> guardMeta(metaLock);
       |      std::lock_guard<std::mutex> guardCommit(commitLock);
       |      // clear old insts from last cycle
       |      lastCommittedInsts.clear();
       |      for (auto it = metas.begin(); it != metas.end() && it->first <= last_commitSN; it = metas.erase(it));
       |      // dump last commmitted insts
       |      for (auto it = metas.begin(); it != metas.end() && it->first <= commitSN; it++) {
       |        for (size_t uopidx = 0; uopidx < it->second.uopTick.size(); uopidx++) {
       |          ss << sql_insert_cmd;
       |          ss << it->second.sn << "," << uopidx;
       |          for (auto pos_it = it->second.uopTick[uopidx].begin();
       |            pos_it != it->second.uopTick[uopidx].end();
       |            pos_it++) {
       |            ss << "," << *pos_it;
       |          }
       |          ss << "," << it->second.instcode;
       |          ss << "," << it->second.pc; 
       |          ss << ");";
       |
       |          id ++;
       |
       |          if (it->second.isload) {
       |            ss << ld_insert_cmd;
       |            ss << id << ',';
       |            ss << it->second.vaddr << ',';
       |            ss << it->second.paddr << ',';
       |            ss << it->second.lastReplay << ',';
       |            ss << '\\'' << it->second.replayStr.str() << '\\'';
       |            ss << ");";
       |          }
       |
       |          rc = sqlite3_exec(mem_db, ss.str().c_str(), callback_temp, 0, &zErrMsg);
       |          if (rc != SQLITE_OK) {
       |            printf("commitMeta SQL error: %s\\n", zErrMsg);
       |            exit(1);
       |          }
       |          ss.str(std::string());
       |        }
       |        if (it->second.uopTick[0].at(AtCommit) != 0)
       |          lastCommittedInsts.push_back(&(it->second));
       |      }
       |      last_commitSN = commitSN;
       |    }
       |#endif
       |  }
       |
       |  uint64_t createInstMeta(uint64_t order_id, uint64_t pc, uint64_t instcode) {
       |#if ${enableCCT.toString()}
       |    if (!enable_dump_lifetime) [[likely]] return 0;
       |
       |    uint64_t sn = last_max_sn + order_id;
       |    sn_acc = std::max(sn_acc, sn);
       |    auto meta = getMeta(sn);
       |    meta->second.reset(sn, pc, instcode);
       |    meta->second.updateUopTick(0, AtFetch, global_tick_acc);
       |
       |    return sn;
       |#endif
       |    return 0;
       |  }
       |
       |  void updateInstPos(uint64_t sn, uint64_t uopIdx, const InstPos pos) {
       |#if ${enableCCT.toString()}
       |    if (!enable_dump_lifetime) [[likely]] return;
       |
       |    auto meta = getMeta(sn);
       |    if (meta == metas.end()) [[unlikely]] return;
       |    meta->second.updateUopTick(uopIdx, pos, global_tick_acc);
       |#endif
       |  }
       |
       |  // interface example
       |  void updateInstMeta(uint64_t sn, uint64_t uopIdx, uint64_t detail, uint64_t val) {
       |#if ${enableCCT.toString()}
       |    if (!enable_dump_lifetime) [[likely]] return;
       |     
       |     auto meta = getMeta(sn);
       |     if (meta == metas.end()) [[unlikely]] return;
       |     switch (detail) {
       |       case InstDetail::VAddress: {
       |         meta->second.isload = true;
       |         meta->second.vaddr = val;
       |         break;
       |       }
       |       case InstDetail::PAddress: {
       |         meta->second.paddr = val;
       |         break;
       |       }
       |       case InstDetail::LastReplay:{
       |         meta->second.lastReplay = val;
       |         break;
       |       }
       |       case InstDetail::ReplayStr:{
       |         assert(val < sizeof(ReplayReasonStr));
       |         meta->second.replayStr << ReplayReasonStr[val];
       |         break;
       |       }
       |     }
       |
       |#endif
       |  }
       |
       |  void commitMeta(uint64_t order_id, uint64_t sn, uint64_t block_size) {
       |#if ${enableCCT.toString()}
       |    if (sn == 0) [[unlikely]] return;
       |
       |    for (int i=0;i<block_size;i++) {
       |      auto meta = getMeta(sn + i);
       |      meta->second.updateUopTick(0, AtCommit, global_tick_acc);
       |    }
       |    std::lock_guard<std::mutex> guardCommit(commitLock);
       |    commitSN = std::max(commitSN, sn + block_size - 1);
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
       |  void updateInstPos(uint64_t sn, uint64_t uopIdx, const InstPos pos) {
       |    perfCCT->updateInstPos(sn, uopIdx, pos);
       |  }
       |
       |  void updateInstMeta(uint64_t sn, uint64_t uopIdx, uint64_t meta, uint64_t data) {
       |    perfCCT->updateInstMeta(sn, uopIdx, meta, data);
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
