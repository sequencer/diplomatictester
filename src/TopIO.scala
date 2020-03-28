package diplomatictester

import chisel3._
import chisel3.experimental._
import firrtl.transforms.DontTouchAnnotation

object TopIO {
  def addIO(data: Data, name: String): Unit = {
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

  def getIO(data: Data, name: String): Unit = {
    data <> DontCare
    Seq(
      new ChiselAnnotation with RunFirrtlTransform {
        def toFirrtl = TopIOAnnotation(data.toTarget, name)

        def transformClass = classOf[TopIOTransform]
      },
      new ChiselAnnotation {
        def toFirrtl = DontTouchAnnotation(data.toTarget)
      }
    ) foreach (annotate(_))
  }
}