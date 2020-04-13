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

class DutIOTest(implicit p: Parameters) extends TLFuzzRAM {
  lazy val module = new LazyModuleImp(this) {
    val dutAuto: AutoBundle = dutIO(ram.module.auto, "dutAuto")
    val dutClock: Clock = dutIO(ram.module.clock, "dutClock")
    val dutReset: Reset = dutIO(ram.module.reset, "dutReset")
  }
}

object DutIOTester extends App {
  implicit val p = Parameters((site, here, up) => {
    case MonitorsEnabled => false
  })
  val lm = LazyModule(new DutIOTest())
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

      c.dutAuto.pokePartial(chiselTypeOf(c.dutAuto).Lit(
        _.elements("out").asInstanceOf[TLBundle].a.bits -> outEdge.PutFullData(size, source, address, mask, corrupt = false, data),
        _.elements("out").asInstanceOf[TLBundle].a.valid -> true.B,
        _.elements("out").asInstanceOf[TLBundle].d.ready -> true.B
      ))
      c.clock.step(1)
      c.dutAuto.pokePartial(chiselTypeOf(c.dutAuto).Lit(
        _.elements("out").asInstanceOf[TLBundle].a.valid -> false.B
      ))
      c.clock.step(5)
      c.dutAuto.pokePartial(chiselTypeOf(c.dutAuto).Lit(
        _.elements("out").asInstanceOf[TLBundle].a.bits -> outEdge.Get(size, source, address, mask),
        _.elements("out").asInstanceOf[TLBundle].a.valid -> true.B
      ))
      c.clock.step(1)
      c.dutAuto.pokePartial(chiselTypeOf(c.dutAuto).Lit(
        _.elements("out").asInstanceOf[TLBundle].a.valid -> false.B
      ))
      c.clock.step(5)
  }
}