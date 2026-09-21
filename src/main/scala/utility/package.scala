import chisel3.{Data, UInt}

package object utility {
  @deprecated
  type SRAMTemplate[T <: Data] = _root_.utility.sram.SRAMTemplate[T]
  @deprecated
  type FoldedSRAMTemplate[T <: Data] = _root_.utility.sram.FoldedSRAMTemplate[T]
  @deprecated
  type SRAMTemplateWithArbiter[T <: Data] = _root_.utility.sram.SRAMTemplateWithArbiter[T]
  @deprecated
  type SRAMBundleAW[T <: Data] = _root_.utility.sram.SRAMBundleAW[T]
  @deprecated
  type SRAMReadBus[T <: Data] = _root_.utility.sram.SRAMReadBus[T]
  @deprecated
  type SRAMWriteBus[T <: Data] = _root_.utility.sram.SRAMWriteBus[T]

  val PerfCCT = utility.TaggedTrace

  implicit class PerfEventDescription[T <: UInt](private val event: (String, T)) extends AnyVal {
    def withDescription(description: String): PerfEventInfo =
      PerfEventInfo(event._1, event._2, description)
  }
}
