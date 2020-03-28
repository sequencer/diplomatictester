package diplomatictester.tests

import diplomatictester._
import chisel3._
import chisel3.util._
import chisel3.stage.ChiselGeneratorAnnotation
import firrtl.options.TargetDirAnnotation

class InnerModule0 extends MultiIOModule {
  val innerIO0 = IO(Decoupled(Bool()))
  TopIO.addIO(innerIO0, "boring_")
}

class InnerModule1 extends MultiIOModule {
  val innerIO1 = IO(Decoupled(Bool()))
  innerIO1.valid := innerIO1.ready
  val r = RegInit(0.U)
  when(innerIO1.fire()) {
    r := r + 1.U
  }
  innerIO1.bits := r
}

class OuterModule extends MultiIOModule {
  val innerInstance = Module(new InnerModule0)
  val anotherInstance = Module(new InnerModule1)
  val outerIO = IO(innerInstance.innerIO0.cloneType)
  val outerIO1 = IO(anotherInstance.innerIO1.cloneType)
  innerInstance.innerIO0 <> outerIO
  anotherInstance.innerIO1 <> outerIO1
}

object Test extends App {
  (new chisel3.stage.ChiselStage).run(Seq(
    ChiselGeneratorAnnotation(() => new OuterModule),
    TargetDirAnnotation("circuit")
  ))
}
