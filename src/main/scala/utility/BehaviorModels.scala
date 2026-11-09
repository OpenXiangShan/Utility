package utility

import chisel3._
import chisel3.experimental.SourceInfo
import chisel3.reflect.DataMirror
import chisel3.util.{HasBlackBoxInline, log2Ceil}
import scala.collection.immutable.ListMap
import org.chipsalliance.cde.config.Field

import java.nio.file.{Files, Path}
import scala.collection.mutable.ArrayBuffer
import scala.util.DynamicVariable

/** Elaboration-time selection of an alternate simulation behavior model. */
case object SimulationModelsKey extends Field[String]("hardware")
case object SimulationModelLoweringKey extends Field[String]("ordinary")
case object SimulationModelComputeLoweringKey extends Field[String]("ordinary")

/** Explicit semantic description for a host-side integer operation. */
final case class HostOpSpec(
  id: String,
  operands: Seq[Int] = Seq.empty,
  resultWidth: Int = 0,
  signed: Boolean = false,
  laneWidth: Int = 0,
  implementation: String = "native",
  shiftMask: Int = -1,
) {
  require(id.nonEmpty, "host operation id must not be empty")
  require(operands.forall(_ > 0), s"host operation '$id' has invalid operand width")
  require(resultWidth >= 0 && laneWidth >= 0, s"host operation '$id' has invalid result/lane width")
  require(Set("native", "fallback").contains(implementation),
    s"host operation '$id' has unknown implementation: $implementation")
  require(shiftMask >= -1, s"host operation '$id' has invalid shift mask")
}

/**
  * A small, source-compatible API for simulation-only state models.
  *
  * The default path builds ordinary Chisel registers and memories. The optional
  * grouped path preserves array operations in a versioned module with complete
  * RTL and typed bindings, so the backend can choose ordinary or grouped state
  * updates. The sidecar never substitutes for actual graph dependencies.
  */
object SimulationModels {
  private val supportedModels = Set(
    "commit-rat", "int-mul", "fp-mul", "vec-int-mul", "vec-fp-mul",
    "int-alu", "int-bku",
    "vec-int-alu-lane", "vec-int-alu-whole",
    "vec-int-pu-lane", "vec-int-pu-whole",
    "vec-int-mac-lane", "vec-int-mac-whole"
  )
  private val vectorBoundaryFamilies = Seq(
    Seq("vec-int-alu-lane", "vec-int-alu-whole"),
    Seq("vec-int-pu-lane", "vec-int-pu-whole"),
    Seq("vec-int-mac-lane", "vec-int-mac-whole")
  )

  def selected(mode: String): Set[String] = {
    if (mode == "hardware") Set.empty
    else {
      val names = mode.split(",", -1).toSeq
      require(names.nonEmpty && names.distinct.size == names.size && names.forall(supportedModels.contains),
        s"Unknown or duplicate simulation model selection: $mode")
      vectorBoundaryFamilies.foreach { family =>
        require(family.intersect(names).size <= 1,
          s"vector behavior boundaries are mutually exclusive: ${family.mkString(",")}")
      }
      require(!(names.contains("vec-int-mul") &&
        vectorBoundaryFamilies.last.intersect(names).nonEmpty),
        "legacy vec-int-mul cannot be combined with a boundary-specific MAC model")
      names.toSet
    }
  }

  private final case class StateEntry(
    name: String,
    kind: String,
    width: Int,
    rows: Int,
    source: String,
  )

  private final case class PortEntry(
    name: String,
    direction: String,
    width: Int,
    source: String,
  )

  private final case class CheckEntry(
    name: String,
    width: Int,
    source: String,
  )

  private final case class UpdateEntry(
    order: Int,
    state: String,
    enable: String,
    kind: String,
    address: String,
    data: String,
    source: String,
  )

  private final class ModelEntry(
    val name: String,
    val module: String,
    val clock: String,
    val reset: String,
    val observerOnly: Boolean,
    val source: String,
    val lowering: String,
  ) {
    val states = ArrayBuffer.empty[StateEntry]
    val inputs = ArrayBuffer.empty[PortEntry]
    val outputs = ArrayBuffer.empty[PortEntry]
    val checks = ArrayBuffer.empty[CheckEntry]
    val capabilities = ArrayBuffer.empty[String]
    val updates = ArrayBuffer.empty[UpdateEntry]
    var commitBoundary: String = ""
    var updateGroup: String = ""
    var binding: String = "null"
    var pipelineLatency: Option[Int] = None
    var pipelineEnable: String = ""
    var productWidths: Seq[(Int, Int)] = Seq.empty
    var computeModule: String = ""
    var computeInputs = ArrayBuffer.empty[PortEntry]
    var computeOutputs = ArrayBuffer.empty[PortEntry]
    var computeBinding: String = "null"
    var computeBoundary: String = "none"
    var computeOps: Seq[HostOpSpec] = Seq.empty
  }

  private final class Scope(val mode: String, val lowering: String, val computeLowering: String) {
    val models = ArrayBuffer.empty[ModelEntry]
    val observers = ArrayBuffer.empty[(RawModule, String, Boolean)]
  }

  private val scope = new DynamicVariable[Option[Scope]](None)

  private def quote(value: String): String = {
    "\"" + value.flatMap {
      case '\\' => "\\\\"
      case '"' => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c if c < ' ' => f"\\u${c.toInt}%04x"
      case c => c.toString
    } + "\""
  }

  private def sourceText(source: SourceInfo): String = source.toString

  private def widthOf(data: Data): Int = {
    val width = data.getWidth
    require(width >= 0, s"behavior model requires a known width, got $width for $data")
    width
  }

  private def sanitize(value: String): String =
    value.map(c => if (c.isLetterOrDigit || c == '_') c else '_').mkString

  /** Run elaboration under one model-selection scope and write its manifest. */
  def withMode[A](mode: String, report: Option[Path] = None, lowering: String = "ordinary",
                  computeLowering: String = "ordinary")(body: => A): A = {
    require(Set("ordinary", "grouped").contains(lowering), s"Unknown behavior lowering: $lowering")
    require(Set("ordinary", "grouped").contains(computeLowering), s"Unknown compute lowering: $computeLowering")
    val selection = selected(mode)
    require(lowering == "ordinary" || selection.contains("commit-rat"), "grouped lowering requires commit-rat")
    val functional = selection.intersect(supportedModels - "commit-rat")
    require(computeLowering == "ordinary" || functional.nonEmpty,
      "grouped compute lowering requires at least one functional model")
    val state = new Scope(mode, lowering, computeLowering)
    val result = scope.withValue(Some(state))(body)
    val registered = state.models.map(_.name).toSet
    val missing = functional -- registered
    require(missing.isEmpty,
      s"selected simulation model(s) were not instantiated during elaboration: ${missing.toSeq.sorted.mkString(",")}")
    report.foreach { path =>
      val models = state.models.map { model =>
        val states = model.states.map { entry =>
          s"{\"name\":${quote(entry.name)},\"kind\":${quote(entry.kind)}," +
            s"\"width\":${entry.width},\"rows\":${entry.rows},\"source\":${quote(entry.source)}}"
        }.mkString(",")
        val inputs = model.inputs.map { entry =>
          s"{\"name\":${quote(entry.name)},\"direction\":${quote(entry.direction)}," +
            s"\"width\":${entry.width},\"source\":${quote(entry.source)}}"
        }.mkString(",")
        val outputs = model.outputs.map { entry =>
          s"{\"name\":${quote(entry.name)},\"direction\":${quote(entry.direction)}," +
            s"\"width\":${entry.width},\"source\":${quote(entry.source)}}"
        }.mkString(",")
        val checks = model.checks.map { entry =>
          s"{\"name\":${quote(entry.name)},\"width\":${entry.width}," +
            s"\"source\":${quote(entry.source)}}"
        }.mkString(",")
        val capabilities = model.capabilities.map(quote).mkString(",")
        val updates = model.updates.map { entry =>
          s"{\"order\":${entry.order},\"state\":${quote(entry.state)}," +
            s"\"enable\":${quote(entry.enable)}," +
            s"\"kind\":${quote(entry.kind)},\"address\":${quote(entry.address)}," +
            s"\"data\":${quote(entry.data)},\"source\":${quote(entry.source)}}"
        }.mkString(",")
        val pipeline = model.pipelineLatency.map { latency =>
          val products = model.productWidths.map { case (a, b) => s"[$a,$b]" }.mkString(",")
          s"{\"latency\":$latency,\"enable_port\":${quote(model.pipelineEnable)}," +
            s"\"reset_semantics\":\"inherited reset for valid; payload has no reset\",\"product_widths\":[$products]}"
        }.getOrElse("null")
        val computePorts = (model.computeInputs ++ model.computeOutputs).map { entry =>
          s"{\"name\":${quote(entry.name)},\"direction\":${quote(entry.direction)}," +
            s"\"width\":${entry.width},\"source\":${quote(entry.source)}}"
        }.mkString(",")
        val computeOps = model.computeOps.map { op =>
          val operands = op.operands.mkString(",")
          s"{\"id\":${quote(op.id)},\"operands\":[$operands],\"result_width\":${op.resultWidth},\"signed\":${op.signed},\"lane_width\":${op.laneWidth},\"implementation\":${quote(op.implementation)},\"shift_mask\":${op.shiftMask}}"
        }.mkString(",")
        s"{\"name\":${quote(model.name)},\"model_version\":1,\"lowering\":${quote(model.lowering)},\"binding\":${model.binding},\"compute_binding\":${model.computeBinding},\"compute_module\":${quote(model.computeModule)},\"compute_boundary\":${quote(model.computeBoundary)},\"compute_ops\":[$computeOps],\"compute_ports\":[$computePorts],\"module\":${quote(model.module)},\"pipeline\":$pipeline," +
          s"\"clock\":${quote(model.clock)},\"reset\":${quote(model.reset)}," +
          s"\"observer_only\":${model.observerOnly}," +
          s"\"commit_boundary\":${quote(model.commitBoundary)}," +
          s"\"update_group\":${quote(model.updateGroup)}," +
          s"\"source\":${quote(model.source)}," +
          s"\"states\":[$states],\"inputs\":[$inputs]," +
          s"\"outputs\":[$outputs],\"checks\":[$checks]," +
          s"\"capabilities\":[$capabilities],\"updates\":[$updates]}"
      }.mkString(",")
      def leaves(name: String, data: Data): Seq[String] = data match {
        case record: Record => record.elements.toSeq.flatMap { case (field, value) => leaves(s"${name}_$field", value) }
        case vector: Vec[_] => vector.zipWithIndex.flatMap { case (value, i) => leaves(s"${name}_$i", value) }.toSeq
        case leaf if leaf.getWidth == 0 => Seq.empty
        case leaf =>
          val direction = DataMirror.directionOf(leaf) match {
            case ActualDirection.Input => "input"
            case ActualDirection.Output => "output"
            case _ => throw new IllegalArgumentException("observer domain requires directional ports")
          }
          Seq(s"{\"name\":${quote(name)},\"width\":${leaf.getWidth},\"direction\":${quote(direction)}}")
      }
      val domains = state.observers.map { case (module, name, children) =>
        val ports = DataMirror.modulePorts(module).flatMap { case (n, d) => leaves(n, d) }.mkString(",")
        s"{\"version\":1,\"id\":${quote(name)},\"module\":${quote(module.name)},\"include_children\":$children,\"ports\":[$ports]}"
      }.mkString(",")
      Option(path.getParent).foreach(Files.createDirectories(_))
      val version = if (lowering == "grouped" || computeLowering == "grouped") 3 else 1
      Files.writeString(path, s"{\"version\":$version,\"mode\":${quote(mode)},\"lowering\":${quote(lowering)},\"compute_lowering\":${quote(computeLowering)},\"observer_domains\":[$domains],\"models\":[$models]}\n")
    }
    result
  }

  def mode: String = scope.value.map(_.mode).getOrElse("hardware")
  def lowering: String = scope.value.map(_.lowering).getOrElse("ordinary")
  def computeLowering: String = scope.value.map(_.computeLowering).getOrElse("ordinary")
  def computeGrouped(model: String): Boolean = enabled(model) && computeLowering == "grouped"

  /** Declare a debug-only module boundary. The importer verifies its typed ports. */
  def observerModule(module: RawModule, name: String, includeChildren: Boolean = true): Unit = {
    if (lowering == "grouped") scope.value.get.observers += ((module, name, includeChildren))
  }

  def enabled(model: String): Boolean = scope.value.exists(s => selected(s.mode).contains(model))

  /** Record a functional arithmetic module; its ordinary RTL owns all dependencies. */
  def functionalPipeline(name: String, module: RawModule, latency: Int,
                         enablePort: String, products: Seq[(Int, Int)],
                         computeModule: Option[RawModule] = None,
                         boundary: String = "none",
                         computeOps: Seq[HostOpSpec] = Seq.empty)(implicit source: SourceInfo): Unit = {
    if (enabled(name)) {
      require(latency >= 0)
      val grouped = computeLowering == "grouped"
      require(!grouped || computeModule.nonEmpty, s"compute-grouped model '$name' needs a pure carrier")
      require(Set("none", "lane-core", "whole-unit").contains(boundary),
        s"unknown compute boundary: $boundary")
      require(boundary == "none" || name.startsWith("vec-int-") || name.startsWith("vec-fp-"),
        s"boundary is only meaningful for vector models: $name")
      val entry = new ModelEntry(name, module.name, "clock", "reset", false, sourceText(source),
        if (grouped) "compute-grouped" else "ordinary")
      entry.pipelineLatency = Some(latency)
      entry.pipelineEnable = enablePort
      entry.productWidths = products
      entry.computeBoundary = boundary
      entry.computeOps = computeOps
      entry.commitBoundary = "enabled rising clock edges"
      entry.capabilities += "functional-arithmetic"
      def record(name: String, data: Data): Unit = data match {
        case r: Record => r.elements.foreach { case (field, value) => record(s"${name}_$field", value) }
        case v: Vec[_] => v.zipWithIndex.foreach { case (value, index) => record(s"${name}_$index", value) }
        case leaf if leaf.getWidth == 0 => ()
        case leaf => DataMirror.directionOf(leaf) match {
          case ActualDirection.Input => entry.inputs += PortEntry(name, "input", widthOf(leaf), sourceText(source))
          case ActualDirection.Output => entry.outputs += PortEntry(name, "output", widthOf(leaf), sourceText(source))
          case _ => throw new IllegalArgumentException("functional pipeline requires directional ports")
        }
      }
      DataMirror.modulePorts(module).foreach { case (port, data) => record(port, data) }
      if (enablePort.nonEmpty) {
        require(entry.inputs.exists(_.name == enablePort), s"missing pipeline enable port $enablePort")
      }
      computeModule.foreach { carrier =>
        entry.computeModule = carrier.name
        def recordCompute(name: String, data: Data): Unit = data match {
          case r: Record => r.elements.foreach { case (field, value) => recordCompute(s"${name}_$field", value) }
          case v: Vec[_] => v.zipWithIndex.foreach { case (value, index) => recordCompute(s"${name}_$index", value) }
          case leaf if leaf.getWidth == 0 => ()
          case leaf => DataMirror.directionOf(leaf) match {
            case ActualDirection.Input => entry.computeInputs += PortEntry(name, "input", widthOf(leaf), sourceText(source))
            case ActualDirection.Output => entry.computeOutputs += PortEntry(name, "output", widthOf(leaf), sourceText(source))
            case _ => throw new IllegalArgumentException("compute carrier requires directional ports")
          }
        }
        DataMirror.modulePorts(carrier).foreach { case (port, data) => recordCompute(port, data) }
        val portSpec = (entry.computeInputs ++ entry.computeOutputs).map(p => s"${p.name}:${p.direction}:${p.width}").mkString("|")
        // The binding carries stable operation IDs; full HostOpSpec details
        // remain in compute_ops in the manifest and are checked separately.
        val operationSpec = computeOps.map(_.id).mkString(";")
        val suffix = if (boundary == "none" && operationSpec.isEmpty) "" else s"|$boundary|$operationSpec"
        val canonical = s"behavior-compute-group-v1|$name|${carrier.name}|$portSpec$suffix"
        val hash = java.security.MessageDigest.getInstance("SHA-256").digest(canonical.getBytes(java.nio.charset.StandardCharsets.UTF_8))
          .map(b => f"${b & 255}%02x").mkString
        val ports = (entry.computeInputs ++ entry.computeOutputs).map { p =>
          s"{\"name\":${quote(p.name)},\"width\":${p.width},\"direction\":${quote(p.direction)}}"
        }.mkString(",")
        val opIds = computeOps.map(op => quote(op.id)).mkString(",")
        entry.computeBinding = s"{\"version\":1,\"kind\":\"compute\",\"module\":${quote(carrier.name)},\"model\":${quote(name)},\"boundary\":${quote(boundary)},\"operations\":[$opIds],\"semantic_sha256\":${quote(hash)},\"ports\":[$ports]}"
      }
      scope.value.get.models += entry
    }
  }

  /** A model body is elaborated with its own explicit clock/reset scope. */
  def model[A](
    name: String,
    clock: Clock,
    reset: Reset,
    observerOnly: Boolean = true,
    commitBoundary: String = "",
    updateGroup: String = "",
    lowering: Option[String] = None,
  )(body: BehaviorModelContext => A)(implicit source: SourceInfo): A = {
    require(enabled(name), s"simulation model '$name' is not enabled (mode=${mode})")
    val entry = new ModelEntry(name, chisel3.XSCompatibility.currentModule.map(_.getClass.getName).getOrElse("unknown"),
      clock.toString, reset.toString, observerOnly, sourceText(source), lowering.getOrElse(SimulationModels.lowering))
    require(Set("ordinary", "grouped").contains(entry.lowering), "unknown model lowering")
    entry.commitBoundary = commitBoundary
    entry.updateGroup = updateGroup
    scope.value.foreach(_.models += entry)
    val context = new BehaviorModelContext(entry)
    withClockAndReset(clock, reset) {
      val result = body(context)
      context.finish(clock, reset)
      result
    }
  }

  /** A typed synchronous register owned by a behavior model. */
  final class BehaviorRegister[T <: Data] private[utility] (
    private[utility] val owner: BehaviorModelContext,
    val name: String,
    private[utility] val value: T,
  ) {
    def read: T = value

    def update(enable: Bool, next: T)(implicit source: SourceInfo): Unit =
      owner.update(this, enable, next, "register", sourceText(source))
  }

  /** A small asynchronous-read memory owned by a behavior model. */
  final class BehaviorMemory[T <: Data] private[utility] (
    private[utility] val owner: BehaviorModelContext,
    val name: String,
    val rows: Int,
    private[utility] val value: Mem[T],
  ) {
    def read(address: UInt): T = value.read(address)

    def update(enable: Bool, address: UInt, next: T)(implicit source: SourceInfo): Unit =
      owner.memoryUpdate(this, enable, address, next)
  }

  /** A resettable, indexable state array for compact behavior-model code. */
  final class BehaviorArray[T <: Data] private[utility] (
    private[utility] val owner: BehaviorModelContext,
    val name: String,
    val rows: Int,
    private[utility] val values: Vec[T],
  ) {
    def read(address: UInt): T = if (owner.preserveArrays) owner.arrayRead(this, address) else WireInit(values(address))

    /** Whole-array observation for a checker or model output. */
    def snapshot: Vec[T] = if (owner.preserveArrays) VecInit((0 until rows).map(i => read(i.U))) else WireInit(values)

    /** Synchronous writes use pre-edge values; the last enabled write to a row wins. */
    def update(enable: Bool, address: UInt, next: T)(implicit source: SourceInfo): Unit =
      owner.arrayUpdate(this, enable, address, next, sourceText(source))
  }

  /** Context exposed to model authors. */
  final class BehaviorModelContext private[utility] (private val entry: ModelEntry) {
    private[utility] val preserveArrays = entry.lowering == "grouped"
    private val arrays = ArrayBuffer.empty[ArraySpec]
    private val reads = ArrayBuffer.empty[(Int, UInt, UInt)]
    private val writes = ArrayBuffer.empty[(Int, Bool, UInt, UInt)]
    private[utility] def arrayRead[T <: Data](state: BehaviorArray[T], address: UInt): T = {
      val index = arrays.indexWhere(_.name == state.name)
      val spec = arrays(index)
      val value = Wire(UInt(spec.width.W))
      val addr = address.pad(spec.addressWidth)(spec.addressWidth - 1, 0)
      reads += ((index, addr, value))
      value.asInstanceOf[T]
    }
    private[utility] def finish(clock: Clock, reset: Reset): Unit = if (preserveArrays) {
      require(entry.observerOnly, "grouped behavior v1 is observer-only")
      require(!reset.isInstanceOf[AsyncReset], "grouped behavior requires synchronous reset")
      require(arrays.nonEmpty, "grouped behavior requires an array")
      val id = s"WolvrixBehaviorStateGroup_v1_${scope.value.get.models.indexOf(entry)}"
      val ports = Seq(("clock", 1, true), ("reset", 1, true)) ++
        writes.zipWithIndex.flatMap { case ((a, _, _, _), i) => Seq(
          (s"u${i}_valid",1,true), (s"u${i}_index",arrays(a).addressWidth,true), (s"u${i}_data",arrays(a).width,true)) } ++
        reads.zipWithIndex.flatMap { case ((a, _, _), i) => Seq(
          (s"r${i}_index",arrays(a).addressWidth,true), (s"r${i}_data",arrays(a).width,false)) }
      val sv = carrierVerilog(id, arrays.toSeq, writes.map(_._1).toSeq, reads.map(_._1).toSeq, ports)
      val module = Module(new ArrayCarrier(id, ports, sv))
      module.io.elements("clock") := clock.asUInt
      module.io.elements("reset") := reset.asBool
      writes.zipWithIndex.foreach { case ((_, valid, index, data), i) =>
        module.io.elements(s"u${i}_valid") := valid
        module.io.elements(s"u${i}_index") := index
        module.io.elements(s"u${i}_data") := data
      }
      reads.zipWithIndex.foreach { case ((_, index, data), i) =>
        module.io.elements(s"r${i}_index") := index
        data := module.io.elements(s"r${i}_data").asUInt
      }
      val states = arrays.map(a => s"{\"name\":${quote(a.name)},\"rows\":${a.rows},\"width\":${a.width},\"initial\":[${a.initial.map(x => quote(x.toString)).mkString(",")}]}" ).mkString(",")
      val bindings = ports.map { case (name,width,input) =>
        s"{\"name\":${quote(name)},\"width\":$width,\"direction\":${quote(if(input) "input" else "output")}}"
      }.mkString(",")
      val hash = java.security.MessageDigest.getInstance("SHA-256").digest(sv.getBytes(java.nio.charset.StandardCharsets.UTF_8))
        .map(b => f"${b & 255}%02x").mkString
      entry.binding = s"{\"version\":1,\"module\":${quote(id)},\"sv_sha256\":${quote(hash)},\"ports\":[$bindings]," +
        s"\"arrays\":[$states],\"updates\":[${writes.map(_._1).mkString(",")}],\"reads\":[${reads.map(_._1).mkString(",")}]}"
    }

    private def addInput(name: String, data: Data, source: SourceInfo): Unit =
      entry.inputs += PortEntry(name, "input", widthOf(data), sourceText(source))

    private def addOutput(name: String, data: Data, source: SourceInfo): Unit =
      entry.outputs += PortEntry(name, "output", widthOf(data), sourceText(source))

    def input[T <: Data](name: String, data: T)(implicit source: SourceInfo): T = {
      addInput(name, data, source)
      data
    }

    def output(name: String, data: Data)(implicit source: SourceInfo): Unit =
      addOutput(name, data, source)

    /** Mark a model output that a checker or adapter must consume. */
    def check(name: String, data: Data)(implicit source: SourceInfo): Unit =
      entry.checks += CheckEntry(name, widthOf(data), sourceText(source))

    /** Declare the debug capability provided by this model. */
    def capability(name: String): Unit = {
      require(name.nonEmpty, "behavior model capability name must not be empty")
      if (!entry.capabilities.contains(name)) entry.capabilities += name
    }

    def register[T <: Data](name: String, init: T)(implicit source: SourceInfo): BehaviorRegister[T] = {
      require(!preserveArrays, "grouped v1 supports array state only")
      val q = RegInit(init)
      q.suggestName(s"behavior_${sanitize(name)}")
      require(widthOf(q) > 0, s"behavior register '$name' requires a positive width")
      entry.states += StateEntry(name, "register", widthOf(q), 1, sourceText(source))
      new BehaviorRegister[T](this, name, q)
    }

    def memory[T <: Data](name: String, rows: Int, gen: T)(implicit source: SourceInfo): BehaviorMemory[T] = {
      require(!preserveArrays, "grouped v1 supports resettable array state only")
      require(rows > 0, s"behavior model memory '$name' must have positive rows")
      require(widthOf(gen) > 0, s"behavior model memory '$name' requires a positive width")
      val mem = Mem(rows, gen.cloneType)
      entry.states += StateEntry(name, "memory", widthOf(gen), rows, sourceText(source))
      new BehaviorMemory[T](this, name, rows, mem)
    }

    /** Declare one indexed state array and update one element atomically. */
    def array[T <: Data](name: String, rows: Int, init: T)(implicit source: SourceInfo): BehaviorArray[T] =
      array(name, rows, (_: Int) => init)

    /** Per-row initialization, for example an identity rename table. */
    def array[T <: Data](name: String, rows: Int, init: Int => T)(implicit source: SourceInfo): BehaviorArray[T] = {
      require(rows > 0, s"behavior model array '$name' must have positive rows")
      require(!entry.states.exists(_.name == name), s"duplicate behavior state '$name'")
      val initial = Seq.tabulate(rows)(init)
      val width = widthOf(initial.head)
      require(width > 0 && initial.forall(widthOf(_) == width),
        s"behavior model array '$name' requires one positive element width")
      val array = if (preserveArrays) {
        require(initial.forall(_.isInstanceOf[UInt]) && width <= 64 && (rows & (rows-1)) == 0,
          "grouped v1 requires power-of-two arrays of 1..64-bit UInt")
        val literals = initial.map(_.asInstanceOf[UInt].litOption.getOrElse(
          throw new IllegalArgumentException("grouped array initialization must be literal")))
        arrays += ArraySpec(name, rows, width, literals)
        null
      } else {
        val value = RegInit(VecInit(initial)); value.suggestName(s"behavior_${sanitize(name)}"); value
      }
      entry.states += StateEntry(name, "array", width, rows, sourceText(source))
      new BehaviorArray[T](this, name, rows, array)
    }

    private[utility] def update[T <: Data](state: BehaviorRegister[T], enable: Bool, next: T,
                                           kind: String, source: String): Unit = {
      when(enable) { state.value := next }
      entry.updates += UpdateEntry(entry.updates.size, state.name, enable.toString, kind, "", next.toString, source)
    }

    private[utility] def memoryUpdate[T <: Data](state: BehaviorMemory[T], enable: Bool,
                                                  address: UInt, next: T)(implicit source: SourceInfo): Unit = {
      when(enable) { state.value.write(address, next) }
      entry.updates += UpdateEntry(entry.updates.size, state.name, enable.toString, "memory", address.toString, next.toString, sourceText(source))
    }

    private[utility] def arrayUpdate[T <: Data](state: BehaviorArray[T], enable: Bool,
                                                 address: UInt, next: T, source: String): Unit = {
      if (preserveArrays) {
        val a = arrays.indexWhere(_.name == state.name); val spec = arrays(a)
        writes += ((a, enable, address.pad(spec.addressWidth)(spec.addressWidth-1,0),
          next.asUInt.pad(spec.width)(spec.width-1,0)))
      } else { when(enable) { state.values(address) := next } }
      entry.updates += UpdateEntry(entry.updates.size, state.name, enable.toString, "array", address.toString, next.toString, source)
    }
  }
  private case class ArraySpec(name: String, rows: Int, width: Int, initial: Seq[BigInt]) {
    val addressWidth: Int = math.max(1, log2Ceil(rows))
  }
  private class CarrierPorts(ports: Seq[(String, Int, Boolean)]) extends Record {
    val elements: ListMap[String, Data] = ListMap.from(ports.map { case (name,width,input) =>
      name -> (if(input) Input(UInt(width.W)) else Output(UInt(width.W)))
    })
  }
  private class ArrayCarrier(id: String, ports: Seq[(String, Int, Boolean)], sv: String)
      extends BlackBox with HasBlackBoxInline {
    override def desiredName: String = id
    val io = IO(new CarrierPorts(ports))
    setInline(s"$id.sv", sv)
  }
  private def carrierVerilog(id: String, arrays: Seq[ArraySpec], updates: Seq[Int], reads: Seq[Int],
      ports: Seq[(String,Int,Boolean)]): String = {
    val text = new StringBuilder
    text.append(s"module $id(\n")
    text.append(ports.map { case (name,width,input) =>
      s"  ${if(input) "input" else "output"} wire [${width-1}:0] $name"
    }.mkString(",\n")); text.append("\n);\n")
    arrays.zipWithIndex.foreach { case (a,i) => text.append(s"reg [${a.width-1}:0] state$i [0:${a.rows-1}];\n") }
    reads.zipWithIndex.foreach { case (a,i) => text.append(s"assign r${i}_data = state$a[r${i}_index & ${arrays(a).rows-1}];\n") }
    text.append("always @(posedge clock) begin\n  if (reset) begin\n")
    arrays.zipWithIndex.foreach { case (a,i) => a.initial.zipWithIndex.foreach { case (v,row) =>
      text.append(s"    state$i[$row] <= ${a.width}'d$v;\n")
    }}
    text.append("  end else begin\n")
    updates.zipWithIndex.foreach { case (a,i) =>
      text.append(s"    if (u${i}_valid) state$a[u${i}_index & ${arrays(a).rows-1}] <= u${i}_data;\n")
    }
    text.append("  end\nend\nendmodule\n").toString
  }

}
