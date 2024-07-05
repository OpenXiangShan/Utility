package utility.mbist
import chisel3._
import utility.ClockGate
import utility.sram.SramBroadcastBundle

class CgDftBundle extends Bundle {
  val ram_mcp_hold = Input(Bool())
  val ram_aux_clk = Input(Bool())
  val ram_aux_ckbp = Input(Bool())
  val cgen = Input(Bool())
  def fromBroadcast(brc: SramBroadcastBundle): Unit = {
    ram_aux_clk := brc.ram_aux_clk
    ram_aux_ckbp := brc.ram_aux_ckbp
    ram_mcp_hold := brc.ram_mcp_hold
    cgen := brc.cgen
  }
}

class MbistClockGateCell(mcpCtl:Boolean) extends Module {
  val mbist = IO(new Bundle {
    val writeen = Input(Bool())
    val readen = Input(Bool())
    val req = Input(Bool())
  })
  val E = IO(Input(Bool()))
  val dft = IO(new CgDftBundle)
  val out_clock = IO(Output(Clock()))

  private val CG = Module(new ClockGate)
  CG.io.TE := dft.cgen
  CG.io.CK := clock

  if(mcpCtl) {
    CG.io.E := Mux(mbist.req, mbist.readen | mbist.writeen, E) && !dft.ram_mcp_hold
    out_clock := Mux(dft.ram_aux_ckbp, dft.ram_aux_clk.asClock, CG.io.Q)
  } else {
    CG.io.E := Mux(mbist.req, mbist.readen | mbist.writeen, E)
    out_clock := CG.io.Q
  }
}
