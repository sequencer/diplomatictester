package diplomatictester

import chisel3._
import chisel3.experimental._
import firrtl.transforms.DontTouchAnnotation
import freechips.rocketchip.diplomacy.ValName

object TopIO {
  @chiselName
  def getIO[T <: Data](data: T, name: String)(implicit valName: ValName): T = {
    val io = IO(DataMirror.directionOf(data) match {
      case ActualDirection.Input => Input(data.cloneType)
      case ActualDirection.Output => Output(data.cloneType)
      case ActualDirection.Bidirectional(ActualDirection.Flipped) => Flipped(data.cloneType)
      case _ => data.cloneType
    })
    io.suggestName(valName.name)
    io <> DontCare
    Seq(
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

  def setIO[T <: Data](data: T, name: String): Unit = {
    data <> DontCare
    Seq(
      new ChiselAnnotation with RunFirrtlTransform {
        def toFirrtl = InnerIOAnnotation(data.toTarget, name)

        def transformClass = classOf[TopIOTransform]
      },
      new ChiselAnnotation {
        def toFirrtl = DontTouchAnnotation(data.toTarget)
      }
    ) foreach (annotate(_))
  }
}