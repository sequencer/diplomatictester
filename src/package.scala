import chisel3.{Data, DiplomaticTester, MultiIOModule}

package object diplomatictester {
  def mockIO[T <: Data]: (T, String) => T = DiplomaticTester.mockIO

  def dutIO[T <: Data]: (T, String) => T = DiplomaticTester.dutIO

  def dutModule[T <: MultiIOModule]: T => Unit = DiplomaticTester.dutModule
}
