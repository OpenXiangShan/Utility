package utility.sram
import chisel3._
import chisel3.util._
import chisel3.experimental.hierarchy.{instantiable, public, Definition, Instance}
import scala.collection.mutable

class SramMbistIO extends Bundle {
  val dft_ram_bypass = Input(Bool())
  val dft_ram_bp_clken = Input(Bool())
}

class SramBroadcastBundle extends Bundle {
  val ram_hold = Input(Bool())
  val ram_bypass = Input(Bool())
  val ram_bp_clken = Input(Bool())
  val ram_aux_clk = Input(Bool())
  val ram_aux_ckbp = Input(Bool())
  val ram_mcp_hold = Input(Bool())
  val cgen = Input(Bool())
}

@instantiable
abstract class SramArray(
  depth:        Int,
  width:        Int,
  maskSegments: Int,
  hasMbist:     Boolean,
  sramName:     Option[String] = None,
  singlePort:   Boolean)
    extends RawModule {
  require(width % maskSegments == 0)
  @public val mbist = if (hasMbist) Some(IO(new SramMbistIO)) else None
  if (mbist.isDefined) {
    dontTouch(mbist.get)
  }

  @public val RW0 = if (singlePort) {
    Some(IO(new Bundle() {
      val clk = Input(Clock())
      val addr = Input(UInt(log2Ceil(depth).W))
      val en = Input(Bool())
      val wmode = Input(Bool())
      val wmask = if (maskSegments > 1) Input(UInt(maskSegments.W)) else Input(UInt(0.W))
      val wdata = Input(UInt(width.W))
      val rdata = Output(UInt(width.W))
    }))
  } else {
    None
  }

  @public val R0 = if (!singlePort) {
    Some(IO(new Bundle() {
      val clk = Input(Clock())
      val addr = Input(UInt(log2Ceil(depth).W))
      val en = Input(Bool())
      val data = Output(UInt(width.W))
    }))
  } else {
    None
  }

  @public val W0 = if (!singlePort) {
    Some(IO(new Bundle() {
      val clk = Input(Clock())
      val addr = Input(UInt(log2Ceil(depth).W))
      val en = Input(Bool())
      val data = Input(UInt(width.W))
      val mask = if (maskSegments > 1) Input(UInt(maskSegments.W)) else Input(UInt(0.W))
    }))
  } else {
    None
  }

  override def desiredName: String = sramName.getOrElse(super.desiredName)
}

@instantiable
class SramArray1P(
  depth:        Int,
  width:        Int,
  maskSegments: Int,
  hasMbist:     Boolean,
  sramName:     Option[String] = None
) extends SramArray(depth, width, maskSegments, hasMbist, sramName, true) {
  if (maskSegments > 1) {
    val dataType = Vec(maskSegments, UInt((width / maskSegments).W))
    val array = SyncReadMem(depth, dataType)
    RW0.get.rdata := array
      .readWrite(
        RW0.get.addr,
        RW0.get.wdata.asTypeOf(dataType),
        RW0.get.wmask.asBools,
        RW0.get.en,
        RW0.get.wmode,
        RW0.get.clk
      )
      .asUInt
  } else {
    val array = SyncReadMem(depth, UInt(width.W))
    RW0.get.rdata := array.readWrite(
      RW0.get.addr,
      RW0.get.wdata,
      RW0.get.en,
      RW0.get.wmode,
      RW0.get.clk
    )
  }
}

@instantiable
class SramArray2P(
  depth:        Int,
  width:        Int,
  maskSegments: Int,
  hasMbist:     Boolean,
  sramName:     Option[String] = None,
) extends SramArray(depth, width, maskSegments, hasMbist, sramName, false) {
  require(width % maskSegments == 0)

  if (maskSegments > 1) {
    val dataType = Vec(maskSegments, UInt((width / maskSegments).W))
    val array = SyncReadMem(depth, dataType, SyncReadMem.WriteFirst)
    when(W0.get.en) {
      array.write(W0.get.addr, W0.get.data.asTypeOf(dataType), W0.get.mask.asBools, W0.get.clk)
    }
    R0.get.data := array.read(R0.get.addr, R0.get.en, R0.get.clk).asUInt
  } else {
    val array = SyncReadMem(depth, UInt(width.W))
    when(W0.get.en) {
      array.write(W0.get.addr, W0.get.data, W0.get.clk)
    }
    R0.get.data := array.read(R0.get.addr, R0.get.en, R0.get.clk)
  }
}

object SramProto {
  private val defMap = mutable.Map[String, Definition[SramArray]]()

  def init(sram: Instance[SramArray], singlePort: Boolean, clock: Clock, writeClock: Option[Clock]): Unit = {
    if (singlePort) {
      dontTouch(sram.RW0.get)
      sram.RW0.get := DontCare
      sram.RW0.get.clk := clock
      sram.RW0.get.en := false.B
    } else {
      dontTouch(sram.R0.get)
      dontTouch(sram.W0.get)
      sram.R0.get := DontCare
      sram.R0.get.clk := clock
      sram.R0.get.en := false.B
      sram.W0.get := DontCare
      sram.W0.get.clk := writeClock.getOrElse(clock)
      sram.W0.get.en := false.B
    }
  }

  def read(sram: Instance[SramArray], singlePort: Boolean, addr: UInt, enable: Bool): UInt = {
    if (singlePort) {
      sram.RW0.get.addr := addr
      sram.RW0.get.en := enable
      sram.RW0.get.wmode := false.B
      sram.RW0.get.rdata
    } else {
      sram.R0.get.addr := addr
      sram.R0.get.en := enable
      sram.R0.get.data
    }
  }

  def write(sram: Instance[SramArray], singlePort: Boolean, addr: UInt, data: UInt, mask: UInt): Unit = {
    if (singlePort) {
      sram.RW0.get.addr := addr
      sram.RW0.get.en := true.B
      sram.RW0.get.wmode := true.B
      if (sram.RW0.get.wmask.getWidth > 1) sram.RW0.get.wmask := mask else sram.RW0.get.wmask := true.B
      sram.RW0.get.wdata := data
    } else {
      sram.W0.get.addr := addr
      sram.W0.get.en := true.B
      if (sram.W0.get.mask.getWidth > 1) sram.W0.get.mask := mask else sram.W0.get.mask := true.B
      sram.W0.get.data := data
    }
  }

  def apply(
    clock:        Clock,
    singlePort:   Boolean,
    depth:        Int,
    width:        Int,
    maskSegments: Int = 1,
    MCP:          Boolean = false,
    writeClock:   Option[Clock] = None,
    hasMbist:     Boolean,
    suffix:       String = ""
  ): (Instance[SramArray], String) = {
    val mbist = if (hasMbist) "_bist" else ""
    val mcpPrefix = if (MCP) "_multicycle" else ""
    val numPort = if (singlePort) 1 else 2
    val maskWidth = width / maskSegments
    val sramName = Some(s"sram_array_${numPort}p${depth}x${width}m$maskWidth$mbist$mcpPrefix$suffix")
    if (!defMap.contains(sramName.get)) {
      val sramDef = if (singlePort) {
        Definition(new SramArray1P(depth, width, maskSegments, hasMbist, sramName))
      } else {
        Definition(new SramArray2P(depth, width, maskSegments, hasMbist, sramName))
      }
      defMap(sramName.get) = sramDef
    }
    val array = Instance(defMap(sramName.get))
    SramProto.init(array, singlePort, clock, writeClock)
    (array, sramName.get)
  }
}
