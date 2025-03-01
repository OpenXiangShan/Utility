package utility.sram

import chisel3._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers

abstract class SRAMSpec extends AnyFlatSpec with Matchers {
  def stepUntilTimeoutCycles = 256

  case class DUTParams(
    entryType: UInt,
    sets: Int,
    ways: Int,
    singlePort: Boolean,
    holdRead: Boolean,
    shouldReset: Boolean
  )

  // extend the module to provide access to parameters within tests
  class DUT(val params: DUTParams, suffix: String) extends SRAMTemplate(
    gen=params.entryType,
    set=params.sets,
    way=params.ways,
    singlePort=params.singlePort,
    holdRead=params.holdRead,
    shouldReset=params.shouldReset,
    suffix=Some(suffix)
  )

  // FIXME: debug why errors occur if we re-use the same suffix in multiple tests (see below)
  // chisel3.package$ChiselException: Imported Definition information not found for sram_array_1p8x32m8s1h0l1_dut - possibly forgot to add ImportDefinition annotation?
  // at ... ()
  // at utility.sram.SramProto$.$anonfun$apply$3(SramProto.scala:226)
  // at chisel3.internal.plugin.package$.autoNameRecursively(package.scala:33)
  // at utility.sram.SramProto$.apply(SramProto.scala:226)
  // at utility.sram.SramHelper$.genRam(SramHelper.scala:145)
  // at utility.sram.SRAMTemplate.$anonfun$x$8$1(SRAMTemplate.scala:247)
  // at chisel3.internal.plugin.package$.autoNameRecursivelyProduct(package.scala:48)
  // at utility.sram.SRAMTemplate.<init>(SRAMTemplate.scala:234)
  var dutCnt = 0
  def makeDut(params: DUTParams) = {
    val dut = new DUT(params, "dut" + dutCnt)
    dutCnt += 1
    dut
  }

  def resetInputs(implicit dut: DUT): Unit = {
    dut.io.r.req.valid.poke(false.B)
    dut.io.w.req.valid.poke(false.B)
  }

  def step(implicit dut: DUT): Unit = {
    dut.clock.step()
    resetInputs
  }

  def stepUntil(expr: => Boolean, timeout: Int = stepUntilTimeoutCycles)(implicit dut: DUT): Unit = {
    var stepCycles = 0
    while (!expr) {
      assert(stepCycles < timeout)
      stepCycles += 1
      dut.clock.step()
    }
  }

  def initialize(implicit dut: DUT): Unit = {
    val writeReq = dut.io.w.req
    if (writeReq.bits.waymask.isDefined) {
      require(writeReq.bits.waymask.get.getWidth == writeReq.bits.data.length)
    }

    resetInputs
    step
  }

  def read(setIdx: UInt)(implicit dut: DUT): Unit = {
    val req = dut.io.r.req
    req.valid.poke(true.B)
    req.bits.setIdx.poke(setIdx)

    stepUntil {req.ready.peek().litToBoolean}
  }

  def getReadResult(implicit dut: DUT): Seq[UInt] = dut.io.r.resp.data.map(_.peek())

  def readResultExpect(values: Seq[UInt], waymask: Option[UInt] = None)(implicit dut: DUT): Unit = {
    val resp = dut.io.r.resp

    require(values.length == resp.data.length)
    if (waymask.isDefined) {
      require(waymask.get.getWidth == resp.data.length)
    }

    val waymaskOrAll = waymask.getOrElse(-1.S(values.length.W).asUInt)
    (resp.data zip values zip waymaskOrAll.asBools) map {
      case ((actual, expected), mask) => if (mask.litToBoolean) actual.expect(expected)
    }
  }

  def readThenExpect(setIdx: UInt, values: Seq[UInt], waymask: Option[UInt] = None)(implicit dut: DUT): Unit = {
    read(setIdx)
    step
    readResultExpect(values, waymask)
  }

  def readThenGet(setIdx: UInt)(implicit dut: DUT): Seq[UInt] = {
    read(setIdx)
    step
    getReadResult
  }

  def write(setIdx: UInt, values: Seq[UInt], waymask: Option[UInt] = None)(implicit dut: DUT): Unit = {
    val req = dut.io.w.req

    require(values.length == req.bits.data.length)
    if (waymask.isDefined) {
      require(req.bits.waymask.isDefined)
      require(waymask.get.getWidth == req.bits.data.length)
    }

    val waymaskOrAll = waymask.getOrElse(-1.S(values.length.W).asUInt)
    req.valid.poke(true.B)
    req.bits.setIdx.poke(setIdx)
    req.bits.waymask.foreach(_.poke(waymaskOrAll))

    (req.bits.data zip values) map {
      case (input, value) => input.poke(value)
    }

    stepUntil {req.ready.peek().litToBoolean}
  }

  def writeStep(setIdx: UInt, values: Seq[UInt], waymask: Option[UInt] = None)(implicit dut: DUT): Unit = {
    write(setIdx, values, waymask)
    step
  }

  def withDut(params: DUTParams, expr: DUT => Unit): Unit = {
    simulate(makeDut(params)) { dut =>
      initialize(dut)
      expr(dut)
    }
  }
}

class SinglePortSRAMSpec extends SRAMSpec {
  val defaultParams = DUTParams(
    entryType = UInt(8.W),
    sets = 8,
    ways = 4,
    singlePort = true,
    holdRead = false,
    shouldReset = false
  )

  def withDut(expr: DUT => Unit): Unit = withDut(defaultParams, expr)

  behavior of "single-port SRAM"

  it should "reject read during write" in {
    withDut { implicit dut =>
      write(0.U, Seq.fill(dut.params.ways)(0.U))
      dut.io.r.req.ready.expect(false.B)
    }
  }

  it should "return written value for write then read" in {
    withDut { implicit dut =>
      for (set <- 0 until dut.params.sets) {
        val data = Seq.fill(dut.params.ways)(set.U)
        writeStep(set.U, data)
        step
        readThenExpect(set.U, data)
      }
    }
  }

  it should "only write to ways with write mask enabled" in {
    withDut { implicit dut =>
      val ways = dut.params.ways
      val dataA = Seq.fill(ways)(3.U)
      val dataB = Seq.fill(ways)(6.U)
      writeStep(0.U, dataA)
      step
      writeStep(0.U, dataB, Some(1.U(ways.W)))
      step
      val dataC = dataB.take(1) ++ dataA.take(ways - 1)
      readThenExpect(0.U, dataC)
    }
  }

  it should "hold read value when holdRead is true" in {
    val holdParams = defaultParams.copy(holdRead = true)
    withDut(holdParams, implicit dut => {
      val dataA = Seq.fill(dut.params.ways)(3.U)
      val dataB = Seq.fill(dut.params.ways)(6.U)
      writeStep(0.U, dataA)
      readThenExpect(0.U, dataA)
      step
      readResultExpect(dataA)
      step
      write(0.U, dataB)
      readResultExpect(dataA)
      step
      readResultExpect(dataA)
    })
  }

  it should "return random value when holdRead is false" in {
    withDut { implicit dut =>
      val data = Seq.fill(dut.params.ways)(3.U)
      writeStep(0.U, data)
      readThenExpect(0.U, data)
      step
      val resultA = getReadResult.map(_.litValue)
      step
      val resultB = getReadResult.map(_.litValue)
      assert(resultA != resultB)
    }
  }

  it should "contain all zero after reset when shouldReset is true" in {
    val resetParams = defaultParams.copy(shouldReset = true)
    withDut(resetParams, implicit dut => {
      for (set <- 0 until dut.params.sets) {
        readThenExpect(set.U, Seq.fill(dut.params.ways)(0.U))
      }
    })
  }

  it should "contain random values after reset when shouldReset is false" in {
    withDut { implicit dut =>
      val data: Seq[Seq[BigInt]] = (0 to 3).map(set => readThenGet(set.U).map(_.litValue))
      assert(!data.forall(_ == data(0)))
    }
  }
}
