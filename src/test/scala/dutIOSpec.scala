package diplomatictester.tests

import chipsalliance.rocketchip.config._
import chiseltest._
import chiseltest.stage.{WaveFormAnnotation, EnableCache, SimulatorBackendAnnotation}
import diplomatictester.Utils._
import firrtl.options.TargetDirAnnotation
import diplomatictester._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import logger._

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
  RawTester.test(lm.module, Seq(
    new WaveFormAnnotation("vcd"),
    LogLevelAnnotation(LogLevel.Info),
    TargetDirAnnotation("./testrun"),
    new EnableCache(true),
    new SimulatorBackendAnnotation("verilator"))) {
    c =>
      val edges: Edges[TLEdgeIn, TLEdgeOut] = lm.ram.node.edges
      val edgeIn: TLEdgeIn = edges.in.head
      val size = 2
      val mask = 0xf
      val source = 0
      val address = 0x150
      val data = BigInt(0x12345678)

      implicit val clock = c.clock
      val in = c.dutAuto.tlBundle("in")
      val a = in.clientA(edgeIn)
      val d = in.clientD(edgeIn)

      /** put data to ram. */
      a.PutFullData(Poke(
        () => println("Putting data"),
        () => println("Put data success")
      ))(size, source, address, mask, false, data).join()

      /** ack from ram. */
      d.AccessAck(Expect(
        () => println("Waiting AccessAck from RAM."),
        () => println(s"Got AccessAck")
      ))(size, source, false).join()
      clock.step()

      /** get data from ram. */
      a.Get(Poke(
        () => println("Getting data from RAM."),
        () => println("Get data success")
      ))(size, source, address, mask).join()

      /** ack data from ram. */
      d.AccessAckData(Expect(
        () => println("Waiting AccessAckData from RAM."),
        () => println("Got AccessAckData")
      ))(size, source, false, false, data).join()
  }
}
