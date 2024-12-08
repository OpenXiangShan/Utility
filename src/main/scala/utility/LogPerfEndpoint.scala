package utility

import chisel3._
import chisel3.util.HasBlackBoxInline
import chisel3.experimental.ExtModule
import org.chipsalliance.cde.config.Parameters

class LogPerfIO extends Bundle {
  val timer = UInt(64.W)
  val logEnable = Bool()
  val clean = Bool()
  val dump = Bool()
}

private class LogPerfEndpoint(infos: Seq[LogPerfParam])(implicit p: Parameters) extends Module {
  val io = IO(Input(new LogPerfIO))
  XSPerfAccumulate.collect(io)
  XSPerfHistogram.collect(io)
  XSPerfMax.collect(io)
  val printer = Module(new LogPerfPrinter(infos))
  printer.clock := clock
  printer.io := io
}

// Implement after circt elaboration
class LogPerfPrinter(infos: Seq[LogPerfParam]) extends ExtModule {
  val clock = IO(Input(Clock()))
  val io = IO(Input(new LogPerfIO))
}

class LogPerfHelper extends BlackBox with HasBlackBoxInline {
  val io = IO(Output(new LogPerfIO))

  val verilog =
    """`ifndef SIM_TOP_MODULE_NAME
      |  `define SIM_TOP_MODULE_NAME SimTop
      |`endif
      |
      |/*verilator tracing_off*/
      |
      |module LogPerfHelper (
      |  output [63:0] timer,
      |  output        logEnable,
      |  output        clean,
      |  output        dump
      |);
      |
      |assign timer         = `SIM_TOP_MODULE_NAME.timer;
      |assign logEnable     = `SIM_TOP_MODULE_NAME.logEnable;
      |assign clean         = `SIM_TOP_MODULE_NAME.clean;
      |assign dump          = `SIM_TOP_MODULE_NAME.dump;
      |
      |endmodule
      |
      |""".stripMargin
  setInline("LogPerfHelper.v", verilog)
}
