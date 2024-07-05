package top
import chisel3._
import utility.SRAMTemplate
import utility.mbist.{MbistInterface, MbistPipeline}
import utility.sram.{SramBroadcastBundle, SramHelper}

class MBISTSubMod extends Module {
  private val sram0 = Module(
    new SRAMTemplate(
      gen = UInt(24.W),
      set = 32,
      way = 1,
      singlePort = true,
      shouldReset = false,
      holdRead = true,
      extraReset = false,
      bypassWrite = false,
      useBitmask = true,
      multicycle = 2,
      hasMbist = true,
      foundry = "TSMC28",
      sramInst = "ts1n28hpcpuhdlvtb32x24m2sw_170a"
    )
  )
  private val sram1 = Module(
    new SRAMTemplate(
      gen = UInt(6.W),
      set = 32,
      way = 2,
      singlePort = false,
      shouldReset = false,
      holdRead = true,
      extraReset = false,
      bypassWrite = true,
      useBitmask = false,
      multicycle = 1,
      hasMbist = true,
      foundry = "TSMC28",
      sramInst = "ts6n28hpcplvta32x6m4sw_130a"
    )
  )
  val io = IO(new Bundle {
    val intf0 = sram0.io.cloneType
    val intf1 = sram1.io.cloneType
  })
  sram0.io <> io.intf0
  sram1.io <> io.intf1
}

class MbistTestTop extends Module {
  private val sramMod = Module(new MBISTSubMod)
  private val pipeline = MbistPipeline.PlaceMbistPipeline(Int.MaxValue, "TopMBISTPipeline").get
  private val intf = Module(new MbistInterface(Seq(pipeline.nodeParams), Seq(pipeline.childrenIds), "TopMBISTIntf", 1))
  pipeline.mbist <> intf.toPipeline.head
  pipeline.registerCSV(intf.info, "MbistTestTop")
  private val broadCasts = SramHelper.genBroadCastBundleTop()

  val io = IO(new Bundle {
    val broadcast = new SramBroadcastBundle
    val sram0 = sramMod.io.intf0.cloneType
    val sram1 = sramMod.io.intf1.cloneType
    val mbist = intf.mbist.cloneType
  })
  broadCasts := io.broadcast
  sramMod.io.intf0 <> io.sram0
  sramMod.io.intf1 <> io.sram1
  intf.mbist <> io.mbist
}
