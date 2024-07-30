package utility

import chisel3._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.interrupts.IntAdapterNode

class IntBuffer(depth: Int = 1)(implicit p: Parameters) extends LazyModule {

  val node = IntAdapterNode()

  lazy val module = new LazyModuleImp(this){
    for(((in, edgeIn), (out, edgeOut)) <- node.in.zip(node.out)){
      out := RegNextN(in, depth, Some(0.U.asTypeOf(in)))
    }
  }

}

object IntBuffer {
  def apply(depth: Int = 1)(implicit p: Parameters) = {
    val intBuffer = LazyModule(new IntBuffer(depth))
    intBuffer.node
  }
}
