package utility

import chisel3._
import chisel3.experimental.{IntParam, SourceInfo, StringParam}
import chisel3.util.BitPat
import chisel3.util.experimental.decode.{Minimizer, QMCMinimizer, EspressoMinimizer, TruthTable}

/** Immutable, self-contained semantic transport. Field zero is packed at the MSB. */
private[utility] object DecodeTable {
  case class Row(mask: BigInt, value: BigInt, out: BigInt, care: BigInt)
  case class Table(mode: String, input: Int, widths: Seq[Int], default: BigInt,
                   defaultCare: BigInt, rows: Seq[Row]) {
    val output: Int = widths.sum
    def payload: String = Seq("WDT1", mode, input.toString, output.toString,
      widths.mkString(","), default.toString(16), defaultCare.toString(16),
      rows.map(r => Seq(r.mask,r.value,r.out,r.care).map(_.toString(16)).mkString(",")).mkString(";")).mkString("|")
    def conflicts: Boolean = {
      if (mode != "T") return false
      val entries = rows.toIndexedSeq
      // Equal masks and distinct keys are disjoint; common for dense tables.
      if (entries.map(_.mask).distinct.size <= 1 && entries.map(_.value).distinct.size == entries.size)
        return false
      entries.indices.exists { i =>
        val b = entries(i)
        entries.iterator.take(i).exists { a =>
          ((a.value ^ b.value) & a.mask & b.mask) == 0 &&
            ((a.out ^ b.out) & a.care & b.care) != 0
        }
      }
    }
  }
  def mask(n: Int): BigInt = (BigInt(1) << n) - 1
  def pack(xs: Seq[BigInt], widths: Seq[Int]): BigInt =
    xs.zip(widths).foldLeft(BigInt(0)) { case (a,(x,w)) => (a << w) | (x & mask(w)) }
  def split(value: UInt, widths: Seq[Int]): List[UInt] = {
    var high = widths.sum
    widths.map { w => high -= w; value(high + w - 1, high) }.toList
  }
  private class Primitive(t: Table) extends RawModule {
    override def desiredName = "WolvrixDecodeTable"
    val in = IO(Input(UInt(t.input.W)))
    val out = IO(Output(UInt(t.output.W)))
    val bytes = t.payload.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    def carrier(n: Int): Int = Seq(1,8,16,32,64).find(_ >= n).getOrElse(n)
    val result = IntrinsicExpr("circt_dpi_call", UInt(carrier(t.output).W),
      "functionName" -> StringParam(s"wolvrix_decode_table_v1_i${t.input}_o${t.output}_b${bytes.length}"),
      "isClocked" -> IntParam(0), "inputNames" -> StringParam("value;table"),
      "outputName" -> StringParam("decoded"))(
        true.B, in.pad(carrier(t.input)), BigInt(1,bytes).U((bytes.length*8).W))
    out := result(t.output-1,0)
  }
  def emit(in: UInt, t: Table)(implicit source: SourceInfo): UInt = {
    val primitive = Module(new Primitive(t))
    primitive.in := in
    primitive.out
  }
}

object ListLookup {
  def apply[T <: Data](addr: UInt, default: List[T], mapping: Array[(BitPat,List[T])])
      (implicit source: SourceInfo): List[T] = {
    val all = default ++ mapping.toList.flatMap(_._2)
    val eligible = addr.isWidthKnown && addr.getWidth > 0 && default.nonEmpty &&
      mapping.forall(_._2.size == default.size) &&
      all.forall(x => x.isInstanceOf[UInt] && x.isLit && x.isWidthKnown && x.getWidth > 0)
    if (!eligible) {
      SimulationPrimitives.selectDecode("ListLookup", addr.widthOption.getOrElse(0), None, "nonliteral-or-unsupported-shape")
      return chisel3.util.ListLookup(addr,default,mapping)
    }
    val widths = default.indices.map(i => (default(i) +: mapping.toSeq.map(_._2(i))).map(_.getWidth).max)
    val outWidth = widths.sum
    val n = addr.getWidth
    val rows = mapping.toSeq.filter { case (p,_) => (p.value >> n) == 0 }.map { case (p,values) =>
      DecodeTable.Row(p.mask & DecodeTable.mask(n),p.value,
        DecodeTable.pack(values.map(_.litValue),widths),DecodeTable.mask(outWidth))
    }
    val t = DecodeTable.Table("P",n,widths,DecodeTable.pack(default.map(_.litValue),widths),DecodeTable.mask(outWidth),rows)
    if (SimulationPrimitives.selectDecode("ListLookup", n, Some(t.payload), "")) {
      DecodeTable.split(DecodeTable.emit(addr,t),widths).zipWithIndex.map { case (value,i) =>
        val boolColumn = default(i).isInstanceOf[Bool] && mapping.forall(_._2(i).isInstanceOf[Bool])
        (if (boolColumn) value.asBool else value).asInstanceOf[T]
      }
    } else chisel3.util.ListLookup(addr,default,mapping)
  }
}

object decoder {
  def apply(minimizer: Minimizer, input: UInt, truthTable: TruthTable)
      (implicit source: SourceInfo): UInt = applyTable(minimizer,input,truthTable,Seq(truthTable.outputWidth),"decoder")
  private[utility] def applyTable(minimizer: Minimizer, input: UInt, truthTable: TruthTable,
                                 widths: Seq[Int], api: String)(implicit source: SourceInfo): UInt = {
    val t = DecodeTable.Table("T",truthTable.inputWidth,widths,
      truthTable.default.value,truthTable.default.mask,truthTable.table.map { case (a,b) =>
        DecodeTable.Row(a.mask,a.value,b.value,b.mask)
      })
    val reason = if (t.input <= 0 || t.output <= 0) "invalid-width"
      else if (t.conflicts) "conflicting-output-constraints" else ""
    if (SimulationPrimitives.selectDecode(api, t.input, Some(t.payload), reason))
      DecodeTable.emit(input,t)
    else chisel3.util.experimental.decode.decoder(minimizer,input,truthTable)
  }
  def apply(input: UInt, truthTable: TruthTable)(implicit source: SourceInfo): UInt = {
    // Keep Chisel's Espresso fallback behavior in the default route.
    val fallback = new Minimizer {
      def minimize(t: TruthTable): TruthTable = try EspressoMinimizer.minimize(t) catch {
        case chisel3.util.experimental.decode.EspressoNotFoundException => QMCMinimizer.minimize(t)
        case _: java.io.IOException => QMCMinimizer.minimize(t)
      }
    }
    apply(fallback,input,truthTable)
  }
  def qmc(input: UInt, table: TruthTable)(implicit source: SourceInfo): UInt = apply(QMCMinimizer,input,table)
  def espresso(input: UInt, table: TruthTable)(implicit source: SourceInfo): UInt = apply(EspressoMinimizer,input,table)
}

/** Same padding/packing convention as Rocket Chip DecodeLogic (LICENSE.Berkeley). */
object DecodeLogic {
  def apply(addr: UInt, default: BitPat, mapping: Iterable[(BitPat,BitPat)])
      (implicit source: SourceInfo): UInt = apply(addr,default,mapping,QMCMinimizer)
  def apply(addr: UInt, default: BitPat, mapping: Iterable[(BitPat,BitPat)], minimizer: Minimizer)
      (implicit source: SourceInfo): UInt = decoder(minimizer,addr,TruthTable(mapping,default))
  def apply(addr: UInt, default: Seq[BitPat], mapping: Iterable[(BitPat,Seq[BitPat])], minimizer: Minimizer = QMCMinimizer)
      (implicit source: SourceInfo): Seq[UInt] = {
    require(mapping.forall(_._2.size == default.size), "DecodeLogic column mismatch")
    val columns = mapping.map(_._2).transpose
    val widths = columns.zip(default).map { case (c,d) => (d :: c.toList).map(_.getWidth).max }.toSeq
    def pad(p: BitPat, w: Int): BitPat = {
      if (p.getWidth == w) p else {
        require(p.mask == DecodeTable.mask(p.getWidth) && w > p.getWidth, "cannot pad don't-care BitPat")
        BitPat(0.U((w-p.getWidth).W)) ## p
      }
    }
    def packed(xs: Seq[BitPat]): BitPat = xs.zip(widths).map { case (p,w) => pad(p,w) }.reduce(_ ## _)
    val table=TruthTable(mapping.map { case (i,o) => i -> packed(o) },packed(default))
    DecodeTable.split(decoder.applyTable(minimizer,addr,table,widths,"DecodeLogic"),widths)
  }
  def apply(addr: UInt, default: Seq[BitPat], mapping: List[(UInt,Seq[BitPat])])
      (implicit source: SourceInfo): Seq[UInt] =
    apply(addr,default,mapping.map { case (i,o) => BitPat(i) -> o }.toSeq: Iterable[(BitPat,Seq[BitPat])])
  def apply(addr: UInt, trues: Iterable[UInt], falses: Iterable[UInt])
      (implicit source: SourceInfo): Bool =
    apply(addr,BitPat.dontCare(1),trues.map(BitPat(_) -> BitPat("b1")) ++ falses.map(BitPat(_) -> BitPat("b0"))).asBool
}
