package diplomatictester

import chisel3._
import chisel3.experimental._
import firrtl.transforms.DontTouchAnnotation

object TopIO {
  /** @todo if possible to remove this name, using val Name for simpler API. */
  def getIO[T <: Data](data: T, name: String): T = {
    val io = IO(DataMirror.directionOf(data) match {
      case ActualDirection.Input => Input(data.cloneType)
      case ActualDirection.Output => Output(data.cloneType)
      case ActualDirection.Bidirectional(ActualDirection.Flipped) => Flipped(data.cloneType)
      case _ => data.cloneType
    })
    io.suggestName(name)
    io <> DontCare
    Seq(
      new ChiselAnnotation with RunFirrtlTransform {
        def toFirrtl = InnerIOAnnotation(data.toTarget, name)

        def transformClass = classOf[TopIOTransform]
      },
      new ChiselAnnotation with RunFirrtlTransform {
        def toFirrtl = TopIOAnnotation(io.toTarget, name)

        def transformClass = classOf[TopIOTransform]
      },
      new ChiselAnnotation {
        def toFirrtl = DontTouchAnnotation(io.toTarget)
      }
    ) foreach (annotate(_))
    io
  }
}