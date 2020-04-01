package diplomatictester.tests

import diplomatictester._
import chisel3._
import chisel3.experimental.DataMirror
import chisel3.util._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util.experimental.BoringUtils
import firrtl.options.TargetDirAnnotation

/** Module to be wired to top. */
class MonitorModule extends MultiIOModule {
  val o = IO(Decoupled(Bool()))
  val i = IO(Flipped(Decoupled(Bool())))
  o := DontCare
  i := DontCare
}

class BufferModule extends MultiIOModule {
  val o = IO(DecoupledIO(Bool()))
  val i = IO(Flipped(DecoupledIO(Bool())))
  val bits = RegNext(i.bits)
  val v = RegNext(i.valid)
  val r = RegNext(o.ready)
  o.bits := bits
  o.valid := v
  i.ready := r
}

class MiddleModule extends MultiIOModule {
  val o = IO(DecoupledIO(Bool()))
  val i = IO(Flipped(DecoupledIO(Bool())))
  val monitorInstance = Module(new MonitorModule)
  val bufferInstance = Module(new BufferModule)
  bufferInstance.i <> monitorInstance.o
  o <> bufferInstance.o
  monitorInstance.i <> i
}

class InnerModule1 extends MultiIOModule {
  val innerIO1 = IO(Decoupled(Bool()))
  innerIO1.valid := innerIO1.ready
  val r = RegInit(Bool())
  when(innerIO1.fire()) {
    r := !r
  }
  innerIO1.bits := r
}

class Top extends MultiIOModule {
  val bufferInstance = Module(new BufferModule)
  val bi = IO(Flipped(DecoupledIO(Bool())))
  val bo = IO(DecoupledIO(Bool()))
  bufferInstance.i <> bi
  bo <> bufferInstance.o

  val middleInstance = Module(new MiddleModule)
  val mi = IO(Flipped(DecoupledIO(Bool())))
  val mo = IO(DecoupledIO(Bool()))
  middleInstance.i <> mi
  middleInstance.o <> mo
  val monitorO = TopIO.getIO(middleInstance.monitorInstance.o, "monitorO")
  val monitorI = TopIO.getIO(middleInstance.monitorInstance.i, "monitorI")
}

object Test extends App {
  (new chisel3.stage.ChiselStage).run(Seq(
    ChiselGeneratorAnnotation(() => new Top),
    TargetDirAnnotation("circuit")
  ))
}
