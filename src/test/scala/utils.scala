package diplomatictester.tests

import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class DummyFuzzer(inFlight: Int = 32)(implicit p: Parameters) extends LazyModule {
  val clientParams = Seq(TLMasterParameters.v1(
    name = "Fuzzer",
    sourceId = IdRange(0, inFlight)
  ))
  val node = TLClientNode(Seq(TLMasterPortParameters.v1(clientParams)))

  lazy val module = new LazyModuleImp(this)
}

abstract class TLFuzzRAM(implicit p: Parameters) extends LazyModule {
  val ram = LazyModule(new TLRAM(AddressSet(0x100, 0xff)))
  val ram2 = LazyModule(new TLRAM(AddressSet(0, 0xff), beatBytes = 16))
  val xbar = LazyModule(new TLXbar)
  val fuzzer = LazyModule(new DummyFuzzer)

  xbar.node := fuzzer.node
  ram2.node := TLFragmenter(16, 256) := xbar.node
  ram.node := TLFragmenter(4, 256) := TLWidthWidget(16) := xbar.node

}
