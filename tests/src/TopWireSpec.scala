package diplomatictester.tests

import chisel3._
import chisel3.util._
import diplomatictester._
import chiseltest._
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import utest._

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
  monitorInstance.i <> i
  monitorInstance.o <> o
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
  val middleInstance = Module(new MiddleModule)
  val mi = IO(Flipped(DecoupledIO(Bool())))
  val mo = IO(DecoupledIO(Bool()))
  middleInstance.i <> mi
  middleInstance.o <> mo
  val monitorO = TopIO.getIO(middleInstance.monitorInstance.o, "monitorO")
  val monitorI = TopIO.getIO(middleInstance.monitorInstance.i, "monitorI")
}

object TopIOSpec extends ChiselUtestTester {
  val tests: Tests = Tests {
    test("wtf") {
      testCircuit(new Top, Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
        dut.monitorO.initSource().setSinkClock(dut.clock)
        dut.monitorO.enqueueNow(true.B)
        parallel(
          dut.monitorO.enqueueNow(false.B),
          dut.mo.expectDequeueNow(true.B)
        )
        dut.mo.expectDequeueNow(false.B)
      }
    }
  }
}