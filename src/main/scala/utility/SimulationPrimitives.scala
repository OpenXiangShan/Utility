package utility

import chisel3._
import chisel3.experimental.{IntParam, SourceInfo, StringParam}
import org.chipsalliance.cde.config.Field
import java.nio.file.{Files, Path}
import scala.collection.mutable.ArrayBuffer
import scala.util.DynamicVariable

case object SimulationPrimitivesKey extends Field[String]("none")

/** Elaboration-scoped choice; the software operation has no runtime state. */
object SimulationPrimitives {
  private case class Entry(api: String, width: Int, layout: String, source: String,
                           module: String, enabled: Boolean, decision: String)
  private case class Scope(mode: String, entries: ArrayBuffer[Entry])
  private val scope = new DynamicVariable[Option[Scope]](None)

  def withMode[A](mode: String, report: Option[Path] = None)(body: => A): A = {
    require(Set("none", "priority-encode").contains(mode), s"Unknown simulation primitives: $mode")
    val state = Scope(mode, ArrayBuffer.empty)
    val result = scope.withValue(Some(state))(body)
    report.foreach { path =>
      def quote(s: String): String = "\"" + s.flatMap {
        case '\\' => "\\\\"; case '"' => "\\\""; case '\n' => "\\n"
        case '\r' => "\\r"; case '\t' => "\\t"
        case c if c < ' ' => f"\\u${c.toInt}%04x"; case c => c.toString
      } + "\""
      val rows = state.entries.zipWithIndex.map { case (e, id) =>
        s"{\"id\":$id,\"api\":${quote(e.api)},\"width\":${e.width}," +
          s"\"layout\":${quote(e.layout)},\"source\":${quote(e.source)}," +
          s"\"module_class\":${quote(e.module)},\"intrinsic\":${e.enabled}," +
          s"\"decision\":${quote(e.decision)}}"
      }
      Option(path.getParent).foreach(Files.createDirectories(_))
      Files.writeString(path, s"{\"version\":1,\"mode\":${quote(mode)},\"calls\":[${rows.mkString(",")}]}\n")
    }
    result
  }

  /**
    * The first layout policy is deliberately conservative. The intrinsic consumes
    * one packed value; a distributed Seq[Bool] would add a gather before the
    * operation, so it keeps the source implementation until a packed-gather
    * contract has been measured separately.
    */
  private[utility] def selected(api: String, width: Int, layout: String = "packed")
      (implicit source: SourceInfo): Boolean = {
    val modeEnabled = scope.value.exists(_.mode == "priority-encode")
    val enabled = modeEnabled && layout == "packed"
    val decision = if (!modeEnabled) "mode-disabled"
                   else if (layout == "packed") "packed-intrinsic"
                   else "distributed-keeps-hardware"
    scope.value.foreach(_.entries += Entry(api, width, layout, source.toString,
      chisel3.XSCompatibility.currentModule.map(_.getClass.getName).getOrElse("unknown"), enabled, decision))
    enabled
  }

  /** CTZ of exactly N bits, returning N for zero. DPI is a transport contract. */
  private class CountTrailingZeros(width: Int) extends RawModule {
    override def desiredName = s"WolvrixCountTrailingZeros$width"
    val in = IO(Input(UInt(width.W)))
    val out = IO(Output(UInt(chisel3.util.log2Ceil(BigInt(width) + 1).W)))
    val carrier = Seq(1, 8, 16, 32, 64).find(_ >= width).getOrElse(width)
    val result = IntrinsicExpr("circt_dpi_call", UInt(32.W),
      "functionName" -> StringParam(s"wolvrix_ctz_v1_w$width"),
      "isClocked" -> IntParam(0), "inputNames" -> StringParam("value"),
      "outputName" -> StringParam("count"))(true.B, in.pad(carrier))
    out := result(chisel3.util.log2Ceil(BigInt(width) + 1) - 1, 0)
  }

  private[utility] def ctz(in: UInt)(implicit source: SourceInfo): UInt = {
    val width = in.getWidth
    require(width > 0, "priority encoding requires a positive known width")
    // A module boundary keeps a caller's when scope out of the intrinsic enable.
    // Flattening later recovers one pure operation with ordinary data dependencies.
    val primitive = Module(new CountTrailingZeros(width))
    primitive.in := in
    primitive.out
  }

  private[utility] def index(in: UInt)(implicit source: SourceInfo): UInt = {
    val width = in.getWidth
    require(width > 0)
    val resultWidth = math.max(1, chisel3.util.log2Ceil(width))
    Mux(in.orR, ctz(in), (width - 1).U)(resultWidth - 1, 0)
  }
}

/** Source-compatible entry point; ordinary hardware generation is the default. */
object PriorityEncoder {
  /** A Vec[Bool] is physically distributed until the caller explicitly packs it. */
  def apply(in: Vec[Bool])(implicit source: SourceInfo): UInt = {
    if (SimulationPrimitives.selected("PriorityEncoder.Vec", in.length, "distributed"))
      SimulationPrimitives.index(chisel3.util.Cat(in.reverse))
    else chisel3.util.PriorityEncoder(in)
  }
  def apply(in: Bits)(implicit source: SourceInfo): UInt = {
    if (SimulationPrimitives.selected("PriorityEncoder.Bits", in.getWidth, "packed"))
      SimulationPrimitives.index(in.asUInt)
    else chisel3.util.PriorityEncoder(in)
  }
  def apply(in: Seq[Bool])(implicit source: SourceInfo): UInt = {
    if (SimulationPrimitives.selected("PriorityEncoder.Seq", in.size, "distributed")) {
      require(in.nonEmpty)
      SimulationPrimitives.index(chisel3.util.Cat(in.reverse))
    } else chisel3.util.PriorityEncoder(in)
  }
}
