package diplomatictester.tests

import Chisel.DecoupledIO
import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest._
import chiseltest.internal.WriteVcdAnnotation
import diplomatictester._
import diplomatictester.TLEdgeLit._
import diplomatictester.TLHelper._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class MockIOTest(implicit p: Parameters) extends TLFuzzRAM {
  lazy val module = new LazyModuleImp(this) {
    val monitor: AutoBundle = mockIO(fuzzer.module.auto, "monitor")
  }
}

object MockIOTester extends App {
  implicit val p = Parameters((site, here, up) => {
    case MonitorsEnabled => false
  })
  val lm = LazyModule(new MockIOTest())
  RawTester.test(lm.module, Seq(WriteVcdAnnotation)) {
    c =>
      val edges: Edges[TLEdgeIn, TLEdgeOut] = lm.fuzzer.node.edges
      val outEdge = edges.out.head
      val size = 1
      val mask = 0xff
      val source = 0
      val address = 0x100
      val data = 0xff

      val clock = c.clock
      val a: DecoupledIO[TLBundleA] = c.monitor.elements("out").asInstanceOf[TLBundle].a
      val aType = chiselTypeOf(a)
      val d: DecoupledIO[TLBundleD] = c.monitor.elements("out").asInstanceOf[TLBundle].d
      val dType = chiselTypeOf(d)

      /** put data to ram. */
      a.pokePartial(aType.Lit(
        _.bits -> outEdge.PutFullData(size, source, address, mask, false, data),
        _.valid -> true.B
      ))
      clock.step(1)
      d.pokePartial(dType.Lit(
        _.ready -> true.B
      ))
      clock.step(1)

      /** ack from ram. */
      d.expectPartial(dType.Lit(
        _.bits -> flip(outEdge).AccessAck(size, source, false),
        _.valid -> true.B
      ))
      a.pokePartial(aType.Lit(
        _.bits -> outEdge.clear(aType),
        _.valid -> false.B
      ))
      d.pokePartial(dType.Lit(
        _.ready -> false.B
      ))
      clock.step(5)

      /** get data from ram. */
      a.pokePartial(aType.Lit(
        _.bits -> outEdge.Get(size, source, address, mask),
        _.valid -> true.B
      ))
      d.pokePartial(dType.Lit(
        _.ready -> true.B
      ))
      clock.step(10)
      /** @todo fixme: ack data from ram. */
//      d.expectPartial(dType.Lit(
//        _.bits -> flip(outEdge).AccessAckData(size, source, false, false, data),
//        _.ready -> true.B
//      ))
  }
}
