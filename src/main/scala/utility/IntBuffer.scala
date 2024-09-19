package utility

import chisel3._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.interrupts.IntAdapterNode
import freechips.rocketchip.util.AsyncResetSynchronizerShiftReg

class IntBuffer(depth: Int = 1, cdc: Boolean = false)(implicit p: Parameters) extends LazyModule {

  val node = IntAdapterNode()

  lazy val module = new LazyModuleImp(this){
    for(((in, edgeIn), (out, edgeOut)) <- node.in.zip(node.out)) {
      out := (cdc match {
        case true =>  AsyncResetSynchronizerShiftReg(in, depth, 0)
        case false => RegNextN(in, depth, Some(0.U.asTypeOf(in)))
      })
    }
  }

}

object IntBuffer {
  def apply(depth: Int = 1, cdc: Boolean = false)(implicit p: Parameters) = {
    val intBuffer = LazyModule(new IntBuffer(depth, cdc))
    intBuffer.node
  }
}
