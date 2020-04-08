package diplomatictester.tests

import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest._
import chiseltest.internal.WriteVcdAnnotation
import diplomatictester._
import diplomatictester.TLEdgeLit._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

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

  xbar.node := fuzzer.node
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
  RawTester.test(lm.module, Seq(WriteVcdAnnotation)) {
    c =>
      val edges: Edges[TLEdgeIn, TLEdgeOut] = lm.fuzzer.node.edges
      val outEdge = edges.out.head
      val size = 1
      val mask = 0xff
      val source = 0
      val address = 0x100
      val data = 0xff

      c.clock.setTimeout(0)
      c.clock.step(1)

      c.monitor.poke(chiselTypeOf(c.monitor).Lit(
        _.elements("out").asInstanceOf[TLBundle].a.bits -> outEdge.PutFullData(size, source, address, mask, corrupt = false, data),
        _.elements("out").asInstanceOf[TLBundle].a.valid -> true.B,
        _.elements("out").asInstanceOf[TLBundle].d.ready -> true.B,
      ))
      c.clock.step(1)
      c.monitor.poke(chiselTypeOf(c.monitor).Lit(
        _.elements("out").asInstanceOf[TLBundle].a.valid -> false.B
      ))
      c.clock.step(5)
      c.monitor.poke(chiselTypeOf(c.monitor).Lit(
        _.elements("out").asInstanceOf[TLBundle].a.bits -> outEdge.Get(size, source, address, mask),
        _.elements("out").asInstanceOf[TLBundle].a.valid -> true.B,
      ))
      c.clock.step(1)
      c.monitor.poke(chiselTypeOf(c.monitor).Lit(
        _.elements("out").asInstanceOf[TLBundle].a.valid -> false.B
      ))
      c.clock.step(5)
  }
}
