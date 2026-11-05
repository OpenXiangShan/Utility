package utility

import chisel3._
import chisel3.experimental.SourceInfo
import org.chipsalliance.cde.config.Field

import java.nio.file.{Files, Path}
import scala.collection.mutable.ArrayBuffer
import scala.util.DynamicVariable

/** Elaboration-time selection of an alternate simulation behavior model. */
case object SimulationModelsKey extends Field[String]("hardware")

/**
  * A small, source-compatible API for simulation-only state models.
  *
  * The API deliberately builds ordinary Chisel registers and memories.  This
  * keeps the first lowering path visible to FIRRTL/GRH and lets the backend
  * use its existing state scheduling and change detection.  The sidecar
  * manifest describes the model boundary and its capabilities; it is not a
  * substitute for the actual data dependencies.
  */
object SimulationModels {
  private val supportedModes = Set("hardware", "commit-rat")

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
    state: String,
    enable: String,
    kind: String,
    source: String,
  )

  private final class ModelEntry(
    val name: String,
    val module: String,
    val clock: String,
    val reset: String,
    val observerOnly: Boolean,
    val source: String,
  ) {
    val states = ArrayBuffer.empty[StateEntry]
    val inputs = ArrayBuffer.empty[PortEntry]
    val outputs = ArrayBuffer.empty[PortEntry]
    val checks = ArrayBuffer.empty[CheckEntry]
    val capabilities = ArrayBuffer.empty[String]
    val updates = ArrayBuffer.empty[UpdateEntry]
    var commitBoundary: String = ""
    var updateGroup: String = ""
  }

  private final class Scope(val mode: String) {
    val models = ArrayBuffer.empty[ModelEntry]
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
    require(width > 0, s"behavior model requires a known positive width, got $width for $data")
    width
  }

  private def sanitize(value: String): String =
    value.map(c => if (c.isLetterOrDigit || c == '_') c else '_').mkString

  /** Run elaboration under one model-selection scope and write its manifest. */
  def withMode[A](mode: String, report: Option[Path] = None)(body: => A): A = {
    require(supportedModes.contains(mode), s"Unknown simulation model mode: $mode")
    val state = new Scope(mode)
    val result = scope.withValue(Some(state))(body)
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
          s"{\"state\":${quote(entry.state)},\"enable\":${quote(entry.enable)}," +
            s"\"kind\":${quote(entry.kind)},\"source\":${quote(entry.source)}}"
        }.mkString(",")
        s"{\"name\":${quote(model.name)},\"module\":${quote(model.module)}," +
          s"\"clock\":${quote(model.clock)},\"reset\":${quote(model.reset)}," +
          s"\"observer_only\":${model.observerOnly}," +
          s"\"commit_boundary\":${quote(model.commitBoundary)}," +
          s"\"update_group\":${quote(model.updateGroup)}," +
          s"\"source\":${quote(model.source)}," +
          s"\"states\":[$states],\"inputs\":[$inputs]," +
          s"\"outputs\":[$outputs],\"checks\":[$checks]," +
          s"\"capabilities\":[$capabilities],\"updates\":[$updates]}"
      }.mkString(",")
      Option(path.getParent).foreach(Files.createDirectories(_))
      Files.writeString(path, s"{\"version\":1,\"mode\":${quote(mode)},\"models\":[$models]}\n")
    }
    result
  }

  def mode: String = scope.value.map(_.mode).getOrElse("hardware")

  def enabled(model: String): Boolean = scope.value.exists(_.mode == model)

  /** A model body is elaborated with its own explicit clock/reset scope. */
  def model[A](
    name: String,
    clock: Clock,
    reset: Reset,
    observerOnly: Boolean = true,
    commitBoundary: String = "",
    updateGroup: String = "",
  )(body: BehaviorModelContext => A)(implicit source: SourceInfo): A = {
    require(enabled(name), s"simulation model '$name' is not enabled (mode=${mode})")
    val entry = new ModelEntry(name, chisel3.XSCompatibility.currentModule.map(_.getClass.getName).getOrElse("unknown"),
      clock.toString, reset.toString, observerOnly, sourceText(source))
    entry.commitBoundary = commitBoundary
    entry.updateGroup = updateGroup
    scope.value.foreach(_.models += entry)
    val context = new BehaviorModelContext(entry)
    withClockAndReset(clock, reset) { body(context) }
  }

  /** A typed synchronous register owned by a behavior model. */
  final class BehaviorRegister[T <: Data] private[utility] (
    private[utility] val owner: BehaviorModelContext,
    val name: String,
    val value: T,
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
    val value: Mem[T],
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
    val values: Vec[T],
  ) {
    def read(address: UInt): T = values(address)

    def update(enable: Bool, address: UInt, next: T)(implicit source: SourceInfo): Unit =
      owner.arrayUpdate(this, enable, address, next, sourceText(source))
  }

  /** Context exposed to model authors. */
  final class BehaviorModelContext private[utility] (private val entry: ModelEntry) {
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
      val q = RegInit(init)
      q.suggestName(s"behavior_${sanitize(name)}")
      entry.states += StateEntry(name, "register", widthOf(q), 1, sourceText(source))
      new BehaviorRegister[T](this, name, q)
    }

    def memory[T <: Data](name: String, rows: Int, gen: T)(implicit source: SourceInfo): BehaviorMemory[T] = {
      require(rows > 0, s"behavior model memory '$name' must have positive rows")
      val mem = Mem(rows, gen.cloneType)
      entry.states += StateEntry(name, "memory", widthOf(gen), rows, sourceText(source))
      new BehaviorMemory[T](this, name, rows, mem)
    }

    /** Declare one indexed state array and update one element atomically. */
    def array[T <: Data](name: String, rows: Int, init: T)(implicit source: SourceInfo): BehaviorArray[T] = {
      require(rows > 0, s"behavior model array '$name' must have positive rows")
      val array = RegInit(VecInit(Seq.fill(rows)(init)))
      array.suggestName(s"behavior_${sanitize(name)}")
      entry.states += StateEntry(name, "array", widthOf(init), rows, sourceText(source))
      new BehaviorArray[T](this, name, rows, array)
    }

    private[utility] def update[T <: Data](state: BehaviorRegister[T], enable: Bool, next: T,
                                           kind: String, source: String): Unit = {
      when(enable) { state.value := next }
      entry.updates += UpdateEntry(state.name, enable.toString, kind, source)
    }

    private[utility] def memoryUpdate[T <: Data](state: BehaviorMemory[T], enable: Bool,
                                                  address: UInt, next: T)(implicit source: SourceInfo): Unit = {
      when(enable) { state.value.write(address, next) }
      entry.updates += UpdateEntry(state.name, enable.toString, "memory", sourceText(source))
    }

    private[utility] def arrayUpdate[T <: Data](state: BehaviorArray[T], enable: Bool,
                                                 address: UInt, next: T, source: String): Unit = {
      when(enable) { state.values(address) := next }
      entry.updates += UpdateEntry(state.name, enable.toString, "array", source)
    }
  }
}
