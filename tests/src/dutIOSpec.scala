package diplomatictester.tests

import Chisel.DecoupledIO
import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest._
import chiseltest.internal.WriteVcdAnnotation
import diplomatictester.TLEdgeLit._
import diplomatictester._
import diplomatictester.TLHelper._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class DutIOTest(implicit p: Parameters) extends TLFuzzRAM {
  lazy val module = new LazyModuleImp(this) {
    dutModule(ram.module)
    val dutAuto: AutoBundle = dutIO(ram.module.auto, "dutAuto")
  }
}

object DutIOTester extends App {
  implicit val p = Parameters((site, here, up) => {
    case MonitorsEnabled => false
  })
  val lm = LazyModule(new DutIOTest())
  RawTester.test(lm.module, Seq(WriteVcdAnnotation)) {
    c =>
      val edges: Edges[TLEdgeIn, TLEdgeOut] = lm.ram.node.edges
      val inEdge: TLEdgeIn = edges.in.head
      val size = 1
      val mask = 0xff
      val source = 0
      val address = 0x100
      val data = 0xff

      val clock = c.clock
      val reset = c.reset
      val a: DecoupledIO[TLBundleA] = c.dutAuto.elements("in").asInstanceOf[TLBundle].a
      val aType = chiselTypeOf(a)
      val d: DecoupledIO[TLBundleD] = c.dutAuto.elements("in").asInstanceOf[TLBundle].d
      val dType = chiselTypeOf(d)
      /** start test now */
      /** after 5 cycles let's start. */
      clock.step(5)
      /** put data to ram. */
      a.pokePartial(aType.Lit(
        _.bits -> flip(inEdge).PutFullData(size, source, address, mask, corrupt = false, data),
        _.valid -> true.B
      ))
      d.pokePartial(dType.Lit(
        _.ready -> true.B
      ))
      /** after another 5 cycles. */
      clock.step(5)

  }
}
