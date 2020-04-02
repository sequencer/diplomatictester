package diplomatictester.tests

import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import chisel3._
import chiseltest._
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import diplomatictester._

/** Create a TLFuzzer with tester2.
  * GetFull
  * PutFull
  * */
class DummyFuzzer(inFlight: Int = 32)(implicit p: Parameters) extends LazyModule {
  val clientParams = Seq(TLMasterParameters.v1(
    name = "Fuzzer",
    sourceId = IdRange(0, inFlight)
  ))
  val node = TLClientNode(Seq(TLMasterPortParameters.v1(clientParams)))

  lazy val module = new LazyModuleImp(this)
}

class TLFuzzRAM(implicit p: Parameters) extends LazyModule {
  val ram = LazyModule(new TLRAM(AddressSet(0x100, 0xff)))
  val ram2 = LazyModule(new TLRAM(AddressSet(0, 0xff), beatBytes = 16))
  val xbar = LazyModule(new TLXbar)
  val fuzzer = LazyModule(new DummyFuzzer)
  val ramModel = LazyModule(new TLRAMModel("TLFuzzRAM"))

  xbar.node := ramModel.node := fuzzer.node
  ram2.node := TLFragmenter(16, 256) := xbar.node
  ram.node := TLFragmenter(4, 256) := TLWidthWidget(16) := xbar.node

  lazy val module = new LazyModuleImp(this) {
    val monitor: AutoBundle = getIO(fuzzer.module.auto, "monitor")
  }
}

object TLFuzzerTester extends App {
  implicit val p = Parameters((site, here, up) => {
    case MonitorsEnabled => false
  })
  val lm = LazyModule(new TLFuzzRAM())
  RawTester.test(lm.module, Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) {
    c =>
      c.clock.setTimeout(0)
      val opcode = 4 // Get
      val param = 0
      val size = 1
      val mask = 0xff
      val source = 0
      val address = 0x100
      val data = 0x100
      val corrupt = false
      c.clock.step(10)
      println(c.monitor.elements)
      val tlBundle = c.monitor.elements("out").asTypeOf(lm.fuzzer.node.out.head._1.cloneType)
      tlBundle.a.bits.opcode.poke(opcode.U)
      tlBundle.a.bits.param.poke(param.U)
      tlBundle.a.bits.size.poke(size.U)
      tlBundle.a.bits.source.poke(source.U)
      tlBundle.a.bits.address.poke(address.U)
      tlBundle.a.bits.mask.poke(mask.U)
      tlBundle.a.bits.data.poke(data.U)
      tlBundle.a.bits.corrupt.poke(corrupt.B)
      c.clock.step(1)
      c.clock.step(10)
  }
}
