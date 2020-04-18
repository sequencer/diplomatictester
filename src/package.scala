import chisel3._
import chisel3.experimental._
import diplomatictester.firrtl._
import firrtl.transforms.DontTouchAnnotation

package object diplomatictester {
  /**
    * [[mockIO]] extract internal module IO to top IO, making external tester be able to send data directly to it.
    *
    * @param data internal hardware name.
    * @param name IO name of val at Top
    *             It will boring internal module up to top, replace the original module with pure IO to Top.
    *             If use [[mockIO]] to mark a module, all output IO should be marked too, input IO is optional for monitor usage.
    **/
  def mockIO[T <: Data](data: T, name: String): T = {
    val io = IO(DataMirror.directionOf(data) match {
      case ActualDirection.Input => Output(data.cloneType)
      case ActualDirection.Output => Input(data.cloneType)
      case ActualDirection.Bidirectional(ActualDirection.Flipped) => data.cloneType
      case ActualDirection.Bidirectional(ActualDirection.Default) => Flipped(data.cloneType)
      case _ => data.cloneType
    })
    io.suggestName(name)
    io <> DontCare
    Seq(
      new ChiselAnnotation with RunFirrtlTransform {
        def toFirrtl = InnerIOAnnotation(data.toTarget, name)

        def transformClass = classOf[MockIOTransform]
      },
      new ChiselAnnotation with RunFirrtlTransform {
        def toFirrtl = TopIOAnnotation(io.toTarget, name)

        def transformClass = classOf[MockIOTransform]
      },
      new ChiselAnnotation {
        def toFirrtl = DontTouchAnnotation(io.toTarget)
      },
      new ChiselAnnotation {
        def toFirrtl = DontTouchAnnotation(data.toTarget)
      }
    ) foreach (annotate(_))
    io
  }

  /**
    * [[dutIO]] extract arbitrary module as DUT, mark all IO to this DUT, making external tester only this a small part of a big circuit.
    *
    * @param data internal hardware name.
    * @param name IO name of val at Top
    *             It will boring internal module up to top, replace the original module with pure IO to Top.
    *             If use [[mockIO]] to mark a module, all output IO should be marked too, input IO is optional for monitor usage.
    **/
  def dutIO[T <: Data](data: T, name: String): T = {
    val io = IO(DataMirror.directionOf(data) match {
      case ActualDirection.Output => Output(data.cloneType)
      case ActualDirection.Input => Input(data.cloneType)
      case ActualDirection.Bidirectional(ActualDirection.Flipped) => Flipped(data.cloneType)
      case ActualDirection.Bidirectional(ActualDirection.Default) => (data.cloneType)
      case _ => data.cloneType
    })
    io.suggestName(name)
    io <> DontCare
    Seq(
      new ChiselAnnotation with RunFirrtlTransform {
        def toFirrtl = InnerIOAnnotation(data.toTarget, name)

        def transformClass = classOf[DutIOTransform]
      },
      new ChiselAnnotation with RunFirrtlTransform {
        def toFirrtl = TopIOAnnotation(io.toTarget, name)

        def transformClass = classOf[DutIOTransform]
      },
      new ChiselAnnotation {
        def toFirrtl = DontTouchAnnotation(io.toTarget)
      },
      new ChiselAnnotation {
        def toFirrtl = DontTouchAnnotation(data.toTarget)
      }
    ) foreach (annotate(_))
    io
  }
  /**
    * [[dutIO]] extract arbitrary module as DUT, mark all IO to this DUT, making external tester only this a small part of a big circuit.
    *
    * @param module module to be marked as DUT.
    **/
  def dutModule[T <: MultiIOModule](module: T) = {
    val currentModule = Module.currentModule.get.asInstanceOf[MultiIOModule]
    Seq(
      new ChiselAnnotation with RunFirrtlTransform {
        def toFirrtl = InnerIOAnnotation(module.clock.toTarget, "clock")

        def transformClass = classOf[DutIOTransform]
      },
      new ChiselAnnotation with RunFirrtlTransform {
        def toFirrtl = TopIOAnnotation(currentModule.clock.toTarget, "clock")

        def transformClass = classOf[DutIOTransform]
      },
      new ChiselAnnotation with RunFirrtlTransform {
        def toFirrtl = InnerIOAnnotation(module.reset.toTarget, "reset")

        def transformClass = classOf[DutIOTransform]
      },
      new ChiselAnnotation with RunFirrtlTransform {
        def toFirrtl = TopIOAnnotation(currentModule.reset.toTarget, "reset")

        def transformClass = classOf[DutIOTransform]
      },

    ) foreach (annotate(_))
  }
}
