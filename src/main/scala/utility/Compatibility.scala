package chisel3 {

  import chisel3.internal._
  import chisel3.experimental.BaseModule

  object XSCompatibility {
    // Return the internal implicit BaseModule
    def currentModule: Option[BaseModule] = Builder.currentModule

    // Return the internal implicit whenContext
    def currentWhen: Option[WhenContext] = Builder.whenStack.headOption
    def currentWhenCond: Bool = if(!currentWhen.isEmpty) Builder.whenStack.head.localCond else true.B
  }
}
