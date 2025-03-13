package chisel3 {

  import chisel3.internal._
  import chisel3.experimental.BaseModule
  import scala.collection.mutable

  object XSCompatibility {
    // Return the internal implicit BaseModule
    def currentModule: Option[BaseModule] = Builder.currentModule

    // Return the internal implicit whenContext
    def currentWhen: Option[WhenContext] = Option.when(mutable.Stack.empty[WhenContext].nonEmpty)(mutable.Stack.empty[WhenContext].top)
  }
}
