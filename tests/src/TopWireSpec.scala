package diplomatictester.tests

import diplomatictester._
import chisel3._
import chisel3.util._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util.experimental.BoringUtils

class Inner extends MultiIOModule {
  val io = IO(Decoupled(Bool()))
  // put driverWire up to top
  // BoringUtils.addIO(someIO, "id")
  addIO(io, "inner")
}

class Outer extends MultiIOModule {
  val inner = Module(new Inner)
  val out = IO(inner.io.cloneType)
  val innerDriver = IO(inner.io.cloneType)
  // BoringUtils.getIO(someIO, "id")
  BoringUtils.addSource(innerDriver, "inner")
  inner.io <> out
}

object Test extends App {
  (new chisel3.stage.ChiselStage).run(Seq(ChiselGeneratorAnnotation(() => new Outer)))
}
