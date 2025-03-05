package utility.sram

import chisel3._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers
import SRAMConflictBehavior._

abstract class SRAMSpec extends AnyFlatSpec with Matchers {
  def stepUntilTimeoutCycles = 256

  case class DUTParams(
    entryType: UInt,
    sets: Int,
    ways: Int,
    singlePort: Boolean,
    holdRead: Boolean,
    shouldReset: Boolean,
    conflictBehavior: SRAMConflictBehavior
  )

  // extend the module to provide access to parameters within tests
  class DUT(val params: DUTParams, suffix: String) extends SRAMTemplate(
    gen=params.entryType,
    set=params.sets,
    way=params.ways,
    singlePort=params.singlePort,
    holdRead=params.holdRead,
    shouldReset=params.shouldReset,
    conflictBehavior=params.conflictBehavior,
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
    dut.reset.poke(true.B)
    step
    dut.reset.poke(false.B)
  }

  def read(setIdx: UInt, waitReady: Boolean = true)(implicit dut: DUT): Unit = {
    val req = dut.io.r.req
    req.valid.poke(true.B)
    req.bits.setIdx.poke(setIdx)

    if (waitReady) stepUntil {req.ready.peek().litToBoolean}
  }

  def getReadResult(implicit dut: DUT): Seq[BigInt] = dut.io.r.resp.data.map(_.peek().litValue)

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

  def readThenGet(setIdx: UInt)(implicit dut: DUT): Seq[BigInt] = {
    read(setIdx)
    step
    getReadResult
  }

  def write(setIdx: UInt, values: Seq[UInt], waymask: Option[UInt] = None, waitReady: Boolean = true)(implicit dut: DUT): Unit = {
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

    if (waitReady) stepUntil {req.ready.peek().litToBoolean}
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

abstract class CommonSRAMSpec(readableName: String) extends SRAMSpec {
  def defaultParams: DUTParams
  def withDut(expr: DUT => Unit): Unit = withDut(defaultParams, expr)

  behavior of readableName

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
    withDut(holdParams, { implicit dut =>
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

  // There is no way currently to enable randomization through svsim interface
  // it should "return random value when holdRead is false" in {
  //   withDut { implicit dut =>
  //     val data = Seq.fill(dut.params.ways)(3.U)
  //     writeStep(0.U, data)
  //     readThenExpect(0.U, data)
  //     step
  //     val resultA = getReadResult
  //     step
  //     val resultB = getReadResult
  //     assert(resultA != resultB)
  //   }
  // }

  it should "contain all zero after reset when shouldReset is true" in {
    val resetParams = defaultParams.copy(shouldReset = true)
    withDut(resetParams, { implicit dut =>
      for (set <- 0 until dut.params.sets) {
        readThenExpect(set.U, Seq.fill(dut.params.ways)(0.U))
      }
    })
  }

  // There is no way currently to enable randomization through svsim interface
  // it should "contain random values after reset when shouldReset is false" in {
  //   withDut { implicit dut =>
  //     val data: Seq[Seq[BigInt]] = (0 to 3).map(set => readThenGet(set.U))
  //     assert(!data.forall(_ == data(0)))
  //   }
  // }
}

class SinglePortSRAMSpec extends CommonSRAMSpec("single-port SRAM") {
  override def defaultParams = DUTParams(
    entryType = UInt(8.W),
    sets = 8,
    ways = 4,
    singlePort = true,
    holdRead = false,
    shouldReset = false,
    conflictBehavior = CorruptReadWay
  )

  it should "reject read during write" in {
    withDut { implicit dut =>
      write(0.U, Seq.fill(dut.params.ways)(0.U))
      dut.io.r.req.ready.expect(false.B)
    }
  }
}

class DualPortSRAMSpec extends CommonSRAMSpec("dual-port SRAM") {
  override def defaultParams = DUTParams(
    entryType = UInt(8.W),
    sets = 8,
    ways = 4,
    singlePort = false,
    holdRead = false,
    shouldReset = false,
    conflictBehavior = CorruptReadWay
  )

  def withBehavior(conflictBehavior: SRAMConflictBehavior, expr: DUT => Unit) = {
    val params = defaultParams.copy(conflictBehavior = conflictBehavior)
    withDut(params, expr)
  }

  def testAcceptConflict(conflictBehavior: SRAMConflictBehavior) = {
    withBehavior(conflictBehavior, { implicit dut =>
      read(0.U)
      write(0.U, Seq.fill(dut.params.ways)(0.U))
      dut.io.r.req.ready.expect(true.B)
      dut.io.w.req.ready.expect(true.B)
    })
  }

  def testBypass(conflictBehavior: SRAMConflictBehavior) = {
    withBehavior(conflictBehavior, { implicit dut =>
      val writeDataA = Seq.fill(dut.params.ways)(1.U)
      writeStep(0.U, writeDataA)
      step

      val writeDataB = Seq.fill(dut.params.ways)(2.U)
      read(0.U)
      writeStep(0.U, writeDataB, Some("b0110".U(dut.params.ways.W)))
      val readData = getReadResult

      assert(readData == Seq(1, 2, 2, 1))
    })
  }

  it should "accept concurrent read and write at different indexes" in {
    // test for all values of conflictBehavior
    SRAMConflictBehavior.values.foreach { conflictBehavior =>
      withBehavior(conflictBehavior, { implicit dut =>
        read(0.U)
        write(1.U, Seq.fill(dut.params.ways)(0.U))
        dut.io.r.req.ready.expect(true.B)
        dut.io.w.req.ready.expect(true.B)
      })
    }
  }

  it should "(CorruptRead) accept concurrent read and write at the same index" in {
    testAcceptConflict(CorruptRead)
  }

  it should "(CorruptRead) return random value during read-write conflict" in {
    withBehavior(CorruptRead, { implicit dut =>
      val writeDataA = Seq.fill(dut.params.ways)(1.U)
      writeStep(0.U, writeDataA)
      step

      val writeDataB = Seq.fill(dut.params.ways)(2.U)
      read(0.U)
      writeStep(0.U, writeDataA)
      val readData = getReadResult

      assert(readData != writeDataA.map(_.litValue))
      assert(readData != writeDataB.map(_.litValue))
      assert(!readData.tail.forall(_ == readData(0)))
    })
  }

  it should "(CorruptReadWay) accept concurrent read and write at the same index" in {
    testAcceptConflict(CorruptReadWay)
  }

  it should "(CorruptReadWay) return random value for written ways and old value for other ways during read-write conflict" in {
    withBehavior(CorruptReadWay, { implicit dut =>
      val writeDataA = Seq.fill(dut.params.ways)(1.U)
      writeStep(0.U, writeDataA)
      step

      val writeDataB = Seq.fill(dut.params.ways)(2.U)
      read(0.U)
      writeStep(0.U, writeDataA, Some("b0110".U(dut.params.ways.W)))
      val readData = getReadResult

      assert(readData(0) == 1)
      assert(readData(1) != 1 && readData(1) != 2)
      assert(readData(2) != 1 && readData(2) != 2)
      assert(readData(3) == 1)
      assert(readData(1) != readData(2))
    })
  }

  it should "(BypassWrite) accept concurrent read and write at the same index" in {
    testAcceptConflict(BypassWrite)
  }

  it should "(BypassWrite) return new value for written ways and old value for other ways during read-write conflict" in {
    testBypass(BypassWrite)
  }

  it should "(BufferWrite) accept concurrent read and write at the same index" in {
    testAcceptConflict(BufferWrite)
  }

  it should "(BufferWrite) return new value for written ways and old value for other ways during read-write conflict" in {
    testBypass(BufferWrite)
  }

  it should "(BufferWrite) reject read one cycle after read-write conflict" in {
    withBehavior(BufferWrite, { implicit dut =>
      read(0.U)
      writeStep(0.U, Seq.fill(dut.params.ways)(0.U))
      dut.io.r.req.ready.expect(false.B)
    })
  }

  it should "(BufferWrite) accept read two cycles after read-write conflict" in {
    withBehavior(BufferWrite, { implicit dut =>
      read(0.U)
      writeStep(0.U, Seq.fill(dut.params.ways)(0.U))
      step
      dut.io.r.req.ready.expect(true.B)
    })
  }

  it should "(BufferWrite) return new value for read two cycles after read-write conflict" in {
    withBehavior(BufferWrite, { implicit dut =>
      val writeDataA = Seq.fill(dut.params.ways)(1.U)
      writeStep(0.U, writeDataA)
      step

      val writeDataB = Seq.fill(dut.params.ways)(2.U)
      read(0.U)
      writeStep(0.U, writeDataB, Some("b0110".U(dut.params.ways.W)))
      step

      val readData = readThenGet(0.U)
      assert(readData == Seq(1, 2, 2, 1))
    })
  }

  it should "(BufferWrite) reject write one cycle after read-write conflict" in {
    withBehavior(BufferWrite, { implicit dut =>
      read(0.U)
      writeStep(0.U, Seq.fill(dut.params.ways)(0.U))
      dut.io.w.req.ready.expect(false.B)
    })
  }

  it should "(BufferWrite) accept write two cycles after read-write conflict" in {
    withBehavior(BufferWrite, { implicit dut =>
      read(0.U)
      writeStep(0.U, Seq.fill(dut.params.ways)(0.U))
      step
      dut.io.w.req.ready.expect(true.B)
    })
  }

  it should "(BufferWriteLossy) accept concurrent read and write at the same index" in {
    testAcceptConflict(BufferWriteLossy)
  }

  it should "(BufferWriteLossy) return new value for written ways and old value for other ways during read-write conflict" in {
    testBypass(BufferWriteLossy)
  }

  it should "(BufferWriteLossy) accept read one cycle after read-write conflict" in {
    withBehavior(BufferWriteLossy, { implicit dut =>
      read(0.U)
      writeStep(0.U, Seq.fill(dut.params.ways)(0.U))
      dut.io.r.req.ready.expect(true.B)
    })
  }

  it should "(BufferWriteLossy) return new value for read one cycle after read-write conflict" in {
    withBehavior(BufferWriteLossy, { implicit dut =>
      val writeDataA = Seq.fill(dut.params.ways)(1.U)
      writeStep(0.U, writeDataA)
      step

      val writeDataB = Seq.fill(dut.params.ways)(2.U)
      read(0.U)
      writeStep(0.U, writeDataB, Some("b0110".U(dut.params.ways.W)))

      readThenExpect(0.U, Seq(1.U, 2.U, 2.U, 1.U))
    })
  }

  it should "(BufferWriteLossy) accept write one cycle after read-write conflict" in {
    withBehavior(BufferWriteLossy, { implicit dut =>
      read(0.U)
      writeStep(0.U, Seq.fill(dut.params.ways)(0.U))
      dut.io.w.req.ready.expect(true.B)
    })
  }

  it should "(BufferWriteLossy) return new value for write then read at different index after read-write conflict" in {
    withBehavior(BufferWriteLossy, { implicit dut =>
      val writeDataA = Seq.fill(dut.params.ways)(1.U)
      writeStep(0.U, writeDataA)
      step

      val writeDataB = Seq.fill(dut.params.ways)(2.U)
      read(1.U)
      writeStep(1.U, writeDataB)

      val writeDataC = Seq.fill(dut.params.ways)(3.U)
      writeStep(0.U, writeDataC, Some("b0110".U(dut.params.ways.W)))

      val readData = Seq(1.U, 3.U, 3.U, 1.U)
      readThenExpect(0.U, readData)
      readThenExpect(0.U, readData)
    })
  }

  it should "(BufferWriteLossy) return new value for write then read at same index after read-write conflict" in {
    withBehavior(BufferWriteLossy, { implicit dut =>
      val writeDataA = Seq.fill(dut.params.ways)(1.U)
      writeStep(0.U, writeDataA)
      step

      val writeDataB = Seq.fill(dut.params.ways)(2.U)
      read(0.U)
      writeStep(0.U, writeDataB)

      val writeDataC = Seq.fill(dut.params.ways)(3.U)
      writeStep(0.U, writeDataC, Some("b0110".U(dut.params.ways.W)))

      val readData = Seq(2.U, 3.U, 3.U, 2.U)
      readThenExpect(0.U, readData)
      readThenExpect(0.U, readData)
    })
  }

  it should "(BufferWriteLossy) return new values for read after consecutive read-write conflicts at different index" in {
    withBehavior(BufferWriteLossy, { implicit dut =>
      val writeDataA = Seq.fill(dut.params.ways)(1.U)
      val writeDataB = Seq.fill(dut.params.ways)(2.U)
      writeStep(0.U, writeDataA)
      writeStep(1.U, writeDataB)

      val writeDataC = Seq.fill(dut.params.ways)(3.U)
      read(0.U)
      writeStep(0.U, writeDataC, Some("b0110".U(dut.params.ways.W)))

      val writeDataD = Seq.fill(dut.params.ways)(4.U)
      read(1.U)
      writeStep(1.U, writeDataD, Some("b1010".U(dut.params.ways.W)))

      val readDataA = readThenGet(0.U)
      val readDataB = readThenGet(1.U)
      assert(readDataA == Seq(1, 3, 3, 1))
      assert(readDataB == Seq(2, 4, 2, 4))
    })
  }

  it should "(StallWrite) accept read and reject write during read-write conflict" in {
    withBehavior(StallWrite, { implicit dut =>
      stepUntil {dut.io.w.req.ready.peek().litToBoolean}
      step

      val writeData = Seq.fill(dut.params.ways)(0.U)
      read(0.U)
      write(0.U, writeData, waitReady = false)
      dut.io.r.req.ready.expect(true.B)
      dut.io.w.req.ready.expect(false.B)
    })
  }

  it should "(StallRead) reject read and accept write during read-write conflict" in {
    withBehavior(StallRead, { implicit dut =>
      stepUntil {dut.io.w.req.ready.peek().litToBoolean}
      step

      val writeData = Seq.fill(dut.params.ways)(0.U)
      read(0.U)
      write(0.U, writeData, waitReady = false)
      dut.io.r.req.ready.expect(false.B)
      dut.io.w.req.ready.expect(true.B)
    })
  }
}
