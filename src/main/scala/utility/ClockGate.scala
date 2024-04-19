package utils

import chisel3._
import chisel3.util.HasBlackBoxInline

class ClockGate extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val TE = Input(Bool())
    val E  = Input(Bool())
    val CK = Input(Clock())
    val Q  = Output(Clock())
  })

  val verilog =
    """
      |module ClockGate (
      |  input  wire TE,
      |  input  wire E,
      |  input  wire CK,
      |  output wire Q
      |);
      |
      |  wire clk_en;
      |  reg  clk_en_reg;
      |
      |  assign clk_en = E | TE;
      |
      |`ifdef VCS
      |  always @(CK or clk_en) begin
      |    if (CK == 1'b0)
      |      clk_en_reg <= clk_en;
      |  end
      |`else
      |`ifdef VERILATOR_5
      |  always @(CK or clk_en) begin
      |    if (CK == 1'b0)
      |      clk_en_reg <= clk_en;
      |  end
      |`else
      | always @(posedge CK) 
      |   begin
      |     clk_en_reg = clk_en;
      |   end
      |`endif // VERILATOR_5
      |`endif // VCS
      |
      |  assign Q = CK & clk_en_reg;
      |
      |endmodule
      |
      |""".stripMargin
  setInline("ClockGate.v", verilog)
}