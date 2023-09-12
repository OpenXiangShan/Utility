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

class PerfAccumulateHelper extends Module {
  val io = IO(new Bundle {
    val inc = Input(UInt(32.W))
    val name = Input(UInt((8 * 100).W))
  })

  private val inner = Module(new PerfAccumulateHelper_internal)
  inner.io.clock := clock
  inner.io.inc := io.inc
  inner.io.name := io.name

  private class PerfAccumulateHelper_internal extends BlackBox with HasBlackBoxInline {
    val io = IO(new Bundle {
      val clock = Input(Clock())
      val inc = Input(UInt(32.W))
      val name = Input(UInt((8 * 100).W))
    })

    val verilog =
      """
        |import "DPI-C" function int register_perf_accumulate();
        |import "DPI-C" function void handle_perf_accumulate(int id, int inc, string mod, string name);
        |
        |module PerfAccumulateHelper_internal(
        |  input clock,
        |  input [31:0] inc,
        |  input [8*100-1:0] name
        |);
        |
        |reg [31:0] id;
        |string mod = $sformatf("%m");
        |string sname = name;
        |
        |initial begin
        |  id = register_perf_accumulate();
        |end
        |
        |always@(posedge clock) begin
        |  handle_perf_accumulate(id, inc, mod, sname);
        |end
        |
        |endmodule
        |""".stripMargin
    setInline("PerfAccumulateHelper_internal.v", verilog)
  }
}

class PerfHistogramHelper extends Module {
  val io = IO(new Bundle {
    val inc = Input(UInt(32.W))
    val enable = Input(Bool())
    val start = Input(UInt(32.W))
    val stop = Input(UInt(32.W))
    val step = Input(UInt(32.W))
    val nBins = Input(UInt(32.W))
    val left_strict = Input(Bool())
    val right_strict = Input(Bool())
    val name = Input(UInt((8 * 100).W))
  })

  private val inner = Module(new PerfHistogramHelper_internal)
  inner.io.clock := clock
  inner.io.inc := io.inc
  inner.io.enable := io.enable
  inner.io.start := io.start
  inner.io.stop := io.stop
  inner.io.step := io.step
  inner.io.nBins := io.nBins
  inner.io.left_strict := io.left_strict
  inner.io.right_strict := io.right_strict
  inner.io.name := io.name

  private class PerfHistogramHelper_internal extends BlackBox with HasBlackBoxInline {
    val io = IO(new Bundle {
      val clock = Input(Clock())
      val inc = Input(UInt(32.W))
      val enable = Input(Bool())
      val start = Input(UInt(32.W))
      val stop = Input(UInt(32.W))
      val step = Input(UInt(32.W))
      val nBins = Input(UInt(32.W))
      val left_strict = Input(Bool())
      val right_strict = Input(Bool())
      val name = Input(UInt((8 * 100).W))
    })

    val verilog =
      """
        |import "DPI-C" function int register_perf_histogram(int start, int stop, int step);
        |import "DPI-C" function void handle_perf_histogram(int id, int inc, bit enable,
        |                                                   int start, int stop, int step, int nBins,
        |                                                   bit left_strict, bit right_strict,
        |                                                   string mod, string name);
        |
        |module PerfHistogramHelper_internal(
        |  input clock,
        |  input [31:0] inc,
        |  input enable,
        |  input [31:0] start,
        |  input [31:0] stop,
        |  input [31:0] step,
        |  input [31:0] nBins,
        |  input left_strict,
        |  input right_strict,
        |  input [8*100-1:0] name
        |);
        |
        |reg [31:0] id;
        |string mod = $sformatf("%m");
        |string sname = name;
        |
        |initial begin
        |  id = register_perf_histogram(start, stop, step);
        |end
        |
        |always@(posedge clock) begin
        |  handle_perf_histogram(id, inc, enable, start, stop, step, nBins, left_strict, right_strict, mod, sname);
        |end
        |
        |endmodule
        |""".stripMargin
    setInline("PerfHistogramHelper_internal.v", verilog)
  }
}

class PerfMaxHelper extends Module {
  val io = IO(new Bundle {
    val inc = Input(UInt(32.W))
    val enable = Input(Bool())
    val name = Input(UInt((8 * 100).W))
  })

  private val inner = Module(new PerfMaxHelper_internal)
  inner.io.clock := clock
  inner.io.inc := io.inc
  inner.io.enable := io.enable
  inner.io.name := io.name

  private class PerfMaxHelper_internal extends BlackBox with HasBlackBoxInline {
    val io = IO(new Bundle {
      val clock = Input(Clock())
      val inc = Input(UInt(32.W))
      val enable = Input(Bool())
      val name = Input(UInt((8 * 100).W))
    })

    val verilog =
      """
        |import "DPI-C" function int register_perf_max();
        |import "DPI-C" function void handle_perf_max(int id, int inc, bit enable, string mod, string name);
        |
        |module PerfMaxHelper_internal(
        |  input clock,
        |  input [31:0] inc,
        |  input enable,
        |  input [8*100-1:0] name
        |);
        |
        |reg [31:0] id;
        |string mod = $sformatf("%m");
        |string sname = name;
        |
        |initial begin
        |  id = register_perf_max();
        |end
        |
        |always@(posedge clock) begin
        |  handle_perf_max(id, inc, enable, mod, sname);
        |end
        |
        |endmodule
        |""".stripMargin
    setInline("PerfMaxHelper_internal.v", verilog)
  }
}
