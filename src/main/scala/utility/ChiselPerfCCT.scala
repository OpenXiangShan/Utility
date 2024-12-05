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
import chisel3.experimental.StringParam
import chisel3.util._
import freechips.rocketchip.util.DataToAugmentedData
import java.lang
import freechips.rocketchip.amba.ahb.AHBImpMaster.bundle


trait HasDPICUtils extends BlackBox with HasBlackBoxInline {
  var moduleName: String = ""
  def init(args: Bundle) {
    val field = args.elements.map(t => {
      val name = t._1
      val tpes = t._2.getClass.getMethods.map(x => x.getName()).toList
      println(tpes.mkString(","))
      val tpe = classOf[UInt].getMethod("specifiedDirection")
      val is_input = tpe.invoke(t._2) == SpecifiedDirection.Input
      (name, is_input)
    }).toList.reverse.drop(3)

    val ports_input = field.filter(_._2).map(_._1)
    val ports_output = field.filter(!_._2).map(_._1)
    val has_out = ports_output.size != 0
    assert(ports_output.size <= 1)
    val port_output = if (has_out) ports_output.head else ""

    //field(0)._2
    if (ports_input.contains(List("clock", "reset", "en"))) {
      throw new Exception
    }

    val className = this.getClass().getSimpleName()
    moduleName = className + "_DPIC_Helper"
    val dpicFunc = lang.Character.toLowerCase(className.charAt(0)) + className.substring(1)
    val verilog= s"""
      |import "DPI-C" function ${if (has_out) "longint unsigned" else "void"} $dpicFunc
      |(
      |${ports_input.map(x => "input longint unsigned " + x).mkString("  ", ",\n  ", ",")}
      |);
      |
      |module $moduleName(
      |input clock,
      |input reset,
      |input en,
      |${if (has_out) "output reg[63:0]" + port_output + "," else ""}
      |${ports_input.map(x => "input [63:0] " + x).mkString("", ",\n", "")}
      |);
      |  always@(posedge clock) begin
      |    if(en && !reset) begin
      |      ${if (has_out) port_output + "<=" else "  "}$dpicFunc(${ports_input.mkString("", ", ", "")});
      |    end
      |  end
      |endmodule
      |""".stripMargin


    setInline(s"$moduleName.v", verilog)
  }


  override def desiredName: String = moduleName
}

class CreateInstMeta extends HasDPICUtils {
  val io = IO(new Bundle() {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val en = Input(Bool())
    val sn = Output(UInt(64.W))
    val pc = Input(UInt(64.W))
    val instcode = Input(UInt(64.W))
  })
  init(io)
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
    val sn = Input(UInt(64.W))
  })
  init(io)
}

object PerfCCT {
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

  def createInstMetaAtFetch(pc:UInt, code:UInt, en: Bool, clock: Clock, reset: Reset): UInt = {
    if (enableCCT) {
      val m = Module(new CreateInstMeta)
      m.io.clock := clock
      m.io.reset := reset
      m.io.en := en
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

  def CommitInstMeta(sn: UInt, en: Bool, clock: Clock, reset: Reset) {
    if (enableCCT) {
      val m = Module(new CommitInstMeta)
      m.io.clock := clock
      m.io.reset := reset
      m.io.en := en
      m.io.sn := sn
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
      |
      |enum InstPos {
      |  ${InstPos.values.mkString("", ",\n  ", "")}
      |};
      |
      |#endif
      """.stripMargin
  }

    def getCpp: String = {
s"""
// performanceCounter commitTrace

#include "perfCCT.h"
#include "chisel_db.h"
#include <string>
#include <vector>
#include <sstream>

extern sqlite3 *mem_db;
extern char *zErrMsg;
extern int rc;
int callback_temp(void *NotUsed, int argc, char **argv, char **azColName) { return 0; }

// must define these
extern uint64_t global_sim_tick;
extern std::string riscv_disasm(uint64_t code, uint64_t pc);

bool enable_dump_lifetime = false;

class InstMeta
{
    friend class PerfCCT;
    uint64_t sn;
    uint64_t pc;
    uint64_t instcode;
    std::vector<uint64_t> posTick;

  public:
    void reset(uint64_t sn, uint64_t pc, uint64_t instcode) {
      this->sn = sn;
      this->pc = pc;
      this->instcode = instcode;
      posTick.clear();
      posTick.resize(${InstPos.AtCommit.id} + 1, 0);
    }
};

class PerfCCT
{
    const int MaxMetas = 1500;
    uint64_t sn_acc = 1;

    std::vector<InstMeta> metas;

    std::stringstream ss;
    std::string sql_insert_cmd;

    InstMeta* getMeta(uint64_t sn) {
      return &metas[sn%MaxMetas];
    }

  public:
    PerfCCT() {
      #if ${enableCCT.toString()}
      metas.resize(MaxMetas);

      ss << "INSERT INTO LifeTimeCommitTrace(";
      ss << ${InstPos.values.mkString("\"", ",", ",")}${InstRecord.values.mkString("", ",", "\"")};
      ss << ") VALUES (";
      sql_insert_cmd = ss.str();
      ss.str(std::string());

      const char* createTable=
      "CREATE TABLE LifeTimeCommitTrace( \\ 
      ID INTEGER PRIMARY KEY AUTOINCREMENT, \\ 
      ${InstPos.values.mkString("", " INT NOT NULL, \\ \n", " INT NOT NULL, \\ ")}
      DisAsm CHAR(20) NOT NULL, \\ 
      PC INT NOT NULL \\ 
      );";

      rc = sqlite3_exec(mem_db, createTable, callback_temp, 0, &zErrMsg);
      if (rc != SQLITE_OK) {
        printf("PerfCCT SQL error: %s\\n", zErrMsg);
        exit(0);
      }

      #endif
    }

    uint64_t createInstMeta(uint64_t pc, uint64_t instcode) {
      #if ${enableCCT.toString()}

      if (!enable_dump_lifetime) [[likely]] return 0;

      sn_acc++;
      auto old = getMeta(sn_acc);
      old->reset(sn_acc, pc, instcode);
      old->posTick.at(AtFetch) = global_sim_tick;
      return sn_acc;

      #endif

      return 0;
    }

    void updateInstPos(uint64_t sn, const InstPos pos) {
      #if ${enableCCT.toString()}

      if (!enable_dump_lifetime) [[likely]] return;
      
      auto meta = getMeta(sn);
      meta->posTick.at(pos) = global_sim_tick;

      #endif
    }

    void updateInstMeta(uint64_t, uint64_t, uint64_t) {
      #if ${enableCCT.toString()}

      if (!enable_dump_lifetime) [[likely]] return;
      // TODO
  
      #endif
    }

    void commitMeta(uint64_t sn) {
      #if ${enableCCT.toString()}

      if (!enable_dump_lifetime) [[likely]] return;

      auto meta = getMeta(sn);
      meta->posTick.at(AtCommit) = global_sim_tick;
      ss << sql_insert_cmd;
      ss << meta->posTick[0];
      for (auto it = meta->posTick.begin() + 1; it != meta->posTick.end(); it++) {
          ss << "," << *it;
      }
      ss << ",\'" << riscv_disasm(meta->instcode, meta->pc) << "\'";
      ss << "," << meta->pc;
      ss << ");";

      rc = sqlite3_exec(mem_db, ss.str().c_str(), callback_temp, 0, &zErrMsg);
      if (rc != SQLITE_OK) {
        printf("commitMeta SQL error: %s\\n", zErrMsg);
        exit(0);
      }

      ss.str(std::string());
      #endif
    }
}*perfCCT;

    void init_perfcct(char *((*select_db_list)[256]), int select_db_num) {
      perfCCT = new PerfCCT();
      
      const char *table_name = "lifetime";
      for (int idx = 0; idx < select_db_num; idx++) {
        char *str_p = (*select_db_list)[idx];
        int s_idx = 0;
        bool match = true;
        for (; (str_p[s_idx] != '\\0') && (table_name[s_idx] != '\\0'); s_idx ++) {
          if (str_p[s_idx] != table_name[s_idx]) {
            match = false;
            break;
          }
        }
        if (!match || (str_p[s_idx] != '\\0'))  continue;

        enable_dump_lifetime = true;
        break;
      }
    }

extern "C" {
    // name must same as HasDPICUtils subclass
    uint64_t createInstMeta(uint64_t pc, uint64_t instcode) {
      return perfCCT->createInstMeta(pc, instcode);
    }

    void updateInstPos(uint64_t sn, const InstPos pos) {
      perfCCT->updateInstPos(sn, pos);
    }

    void updateInstMeta(uint64_t sn, uint64_t meta, uint64_t data) {
      perfCCT->updateInstMeta(sn, meta, data);
    }

    void commitInstMeta(uint64_t sn) {
      perfCCT->commitMeta(sn);
    }
}
"""
    }

    def addToFileRegisters {
      FileRegisters.add("perfCCT.h", PerfCCT.getCHeader)
      FileRegisters.add("perfCCT.cpp", PerfCCT.getCpp)
    }
}

