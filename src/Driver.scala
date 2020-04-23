package diplomatictester

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util.DecoupledIO
import chiseltest._
import chiseltest.internal.TesterThreadList
import diplomatictester.TLEdgeLit._
import diplomatictester.Utils._
import freechips.rocketchip.tilelink._

trait TestFunc {
  def preHook: () => Unit

  def execute[T <: TLBundleBase](driver: Driver[T])(litValue: T): Unit = {
    preHook()
    this match {
      case Poke(_, _, timeout) => driver.poke(timeout)(litValue)
      case Expect(_, _, timeout) => driver.expect(timeout)(litValue)
    }
    postHook()
  }

  def postHook: () => Unit
}

case class Poke(preHook: () => Unit = () => (), postHook: () => Unit = () => (), timeout: Int = -1) extends TestFunc

case class Expect(preHook: () => Unit = () => (), postHook: () => Unit = () => (), timeout: Int = -1) extends TestFunc

trait Driver[D <: TLBundleBase] {
  implicit val clock: Clock
  val data: DecoupledIO[D]
  val edge: TLEdge
  val channelType = chiselTypeOf(data)

  /** timeout = -1 means expect forever. */
  def expect(timeout: BigInt)(bits: D): TesterThreadList = fork {
    var limit = timeout + 1
    data.pokePartial(channelType.Lit(
      _.ready -> true.B
    ))
    while (!data.peek().valid.litToBoolean) {
      clock.step()
      limit -= 1
      require(limit != 0, s"timeout in expecting ${data.pathName}")
    }
    data.expectPartial(channelType.Lit(
      _.bits -> bits
    ))
    data.pokePartial(channelType.Lit(
      _.ready -> false.B
    ))
  }

  def poke(timeout: BigInt)(bits: D): TesterThreadList = fork {
    var limit = timeout + 1
    data.pokePartial(channelType.Lit(
      _.bits -> bits,
      _.valid -> true.B
    ))
    while (!data.peek().ready.litToBoolean) {
      clock.step()
      limit -= 1
      require(limit != 0, s"timeout in poking ${data.pathName}")
    }
    clock.step()
  }
}

/** Inward TL channel direction. */
trait Inward[D <: TLBundleBase] extends Driver[D] {
  override def poke(timeout: BigInt)(bits: D): TesterThreadList = {
    assert(cond = false, "You can not poke to a inward channel!")
    fork()
  }
}

/** Outward TL channel direction. */
trait Outward[D <: TLBundleBase] extends Driver[D]

/** manager. */
case class ManagerA(data: DecoupledIO[TLBundleA], edge: TLEdgeIn)(implicit val clock: Clock) extends Inward[TLBundleA] {
  def Get(testFunc: TestFunc)(size: Int, source: Int, address: Int, mask: Int) = {
    require(edge.manager.anySupportGet)
    testFunc.execute(this)(edge.flip.Get(size, source, address, mask))
  }

  def PutFullData(testFunc: TestFunc)(size: Int, source: Int, address: Int, mask: Int, corrupt: Boolean, data: BigInt) = {
    require(edge.manager.anySupportPutFull)
    testFunc.execute(this)(edge.flip.PutFullData(size, source, address, mask, corrupt, data))
  }

  def PutPartialData(testFunc: TestFunc)(size: Int, source: Int, address: Int, mask: Int, corrupt: Boolean, data: BigInt) = {
    require(edge.manager.anySupportPutPartial)
    testFunc.execute(this)(edge.flip.PutPartialData(size, source, address, mask, corrupt, data))
  }

  def ArithmeticData(testFunc: TestFunc)(param: ArithmeticDataParam, size: Int, source: Int, address: Int, mask: Int, corrupt: Boolean, data: BigInt) = {
    require(edge.manager.anySupportArithmetic)
    testFunc.execute(this)(edge.flip.ArithmeticData(param, size, source, address, mask, corrupt, data))
  }

  def LogicalData(testFunc: TestFunc)(param: LogicDataParam, size: Int, source: Int, address: Int, mask: Int, corrupt: Boolean, data: BigInt) = {
    require(edge.manager.anySupportLogical)
    testFunc.execute(this)(edge.flip.LogicalData(param, size, source, address, mask, corrupt, data))
  }

  def Intent(testFunc: TestFunc)(param: IntentParam, size: Int, source: Int, address: Int, mask: Int) = {
    require(edge.manager.anySupportHint)
    testFunc.execute(this)(edge.flip.Intent(param, size, source, address, mask))
  }

  def AcquireBlock(testFunc: TestFunc)(param: Permission, size: Int, source: Int, address: Int, mask: Int) = {
    require(edge.manager.anySupportAcquireB)
    testFunc.execute(this)(edge.flip.AcquireBlock(param, size, source, address, mask))
  }

  def AcquirePerm(testFunc: TestFunc)(param: Permission, size: Int, source: Int, address: Int, mask: Int) = {
    require(edge.manager.anySupportAcquireB)
    testFunc.execute(this)(edge.flip.AcquirePerm(param, size, source, address, mask))
  }
}

case class ManagerB(data: DecoupledIO[TLBundleB], edge: TLEdgeIn)(implicit val clock: Clock) extends Outward[TLBundleB] {
  def ProbeBlock(testFunc: TestFunc)(param: Permission, size: Int, source: Int, address: Int, mask: Int) = {
    testFunc.execute(this)(edge.ProbeBlock(param, size, source, address, mask))
  }

  def ProbePerm(testFunc: TestFunc)(param: Permission, size: Int, source: Int, address: Int, mask: Int) =
    testFunc.execute(this)(edge.ProbeBlock(param, size, source, address, mask))

  def Get(testFunc: TestFunc)(size: Int, source: Int, address: Int, mask: Int) =
    testFunc.execute(this)(edge.Get(size, source, address, mask))

  def PutFullData(testFunc: TestFunc)(size: Int, source: Int, address: Int, mask: Int, corrupt: Boolean, data: BigInt) =
    testFunc.execute(this)(edge.PutFullData(size, source, address, mask, corrupt, data))
}

case class ManagerC(data: DecoupledIO[TLBundleC], edge: TLEdgeIn)(implicit val clock: Clock) extends Inward[TLBundleC] {
  def ProbeAck(testFunc: TestFunc)(param: Permission, size: Int, source: Int, address: Int) =
    testFunc.execute(this)(edge.flip.ProbeAck(param, size, source, address))

  def ProbeAckData(testFunc: TestFunc)(param: Permission, size: Int, source: Int, address: Int, corrupt: Boolean, data: BigInt) =
    testFunc.execute(this)(edge.flip.ProbeAckData(param, size, source, address, corrupt, data))

  def Release(testFunc: TestFunc)(param: Permission, size: Int, source: Int, address: Int) =
    testFunc.execute(this)(edge.flip.Release(param, size, source, address))

  def ReleaseData(testFunc: TestFunc)(param: Permission, size: Int, source: Int, address: Int, data: BigInt) =
    testFunc.execute(this)(edge.flip.ReleaseData(param, size, source, address, data))
}

case class ManagerD(data: DecoupledIO[TLBundleD], edge: TLEdgeIn)(implicit val clock: Clock) extends Outward[TLBundleD] {
  def AccessAck(testFunc: TestFunc)(size: Int, source: Int, denied: Boolean) =
    testFunc.execute(this)(edge.AccessAck(size, source, denied))

  def AccessAckData(testFunc: TestFunc)(size: Int, source: Int, denied: Boolean, corrupt: Boolean, data: BigInt) =
    testFunc.execute(this)(edge.AccessAckData(size, source, denied, corrupt, data))

  def HintAck(testFunc: TestFunc)(size: Int, source: Int, denied: Boolean) =
    testFunc.execute(this)(edge.HintAck(size, source, denied))

  def Grant(testFunc: TestFunc)(param: Permission, size: Int, source: Int, sink: Int, denied: Int) =
    testFunc.execute(this)(edge.Grant(param, size, source, sink, denied))

  def GrantData(testFunc: TestFunc)(param: Permission, size: Int, source: Int, sink: Int, denied: Int, corrupt: Boolean, data: BigInt) =
    testFunc.execute(this)(edge.GrantData(param, size, source, sink, denied, corrupt, data))

  def ReleaseAck(testFunc: TestFunc)(size: Int, source: Int) =
    testFunc.execute(this)(edge.ReleaseAck(size, source))
}

case class ManagerE(data: DecoupledIO[TLBundleE], edge: TLEdgeIn)(implicit val clock: Clock) extends Inward[TLBundleE] {
  def GrantAck(testFunc: TestFunc)(sink: Int) =
    testFunc.execute(this)(edge.flip.GrantAck(sink))
}

/** client. */
case class ClientA(data: DecoupledIO[TLBundleA], edge: TLEdgeOut)(implicit val clock: Clock) extends Outward[TLBundleA] {
  def Get(testFunc: TestFunc)(size: Int, source: Int, address: Int, mask: Int) = {
    require(edge.manager.anySupportGet)
    testFunc.execute(this)(edge.Get(size, source, address, mask))
  }

  def PutFullData(testFunc: TestFunc)(size: Int, source: Int, address: Int, mask: Int, corrupt: Boolean, data: BigInt) = {
    require(edge.manager.anySupportPutFull)
    testFunc.execute(this)(edge.PutFullData(size, source, address, mask, corrupt, data))
  }

  def PutPartialData(testFunc: TestFunc)(size: Int, source: Int, address: Int, mask: Int, corrupt: Boolean, data: BigInt) = {
    require(edge.manager.anySupportPutPartial)
    testFunc.execute(this)(edge.PutPartialData(size, source, address, mask, corrupt, data))
  }

  def ArithmeticData(testFunc: TestFunc)(param: ArithmeticDataParam, size: Int, source: Int, address: Int, mask: Int, corrupt: Boolean, data: BigInt) = {
    require(edge.manager.anySupportArithmetic)
    testFunc.execute(this)(edge.ArithmeticData(param, size, source, address, mask, corrupt, data))
  }

  def LogicalData(testFunc: TestFunc)(param: LogicDataParam, size: Int, source: Int, address: Int, mask: Int, corrupt: Boolean, data: BigInt) = {
    require(edge.manager.anySupportLogical)
    testFunc.execute(this)(edge.LogicalData(param, size, source, address, mask, corrupt, data))
  }

  def Intent(testFunc: TestFunc)(param: IntentParam, size: Int, source: Int, address: Int, mask: Int) = {
    require(edge.manager.anySupportHint)
    testFunc.execute(this)(edge.Intent(param, size, source, address, mask))
  }

  def AcquireBlock(testFunc: TestFunc)(param: Permission, size: Int, source: Int, address: Int, mask: Int) = {
    require(edge.manager.anySupportAcquireB)
    testFunc.execute(this)(edge.AcquireBlock(param, size, source, address, mask))
  }

  def AcquirePerm(testFunc: TestFunc)(param: Permission, size: Int, source: Int, address: Int, mask: Int) = {
    require(edge.manager.anySupportAcquireB)
    testFunc.execute(this)(edge.AcquirePerm(param, size, source, address, mask))
  }
}

case class ClientB(data: DecoupledIO[TLBundleB], edge: TLEdgeOut)(implicit val clock: Clock) extends Inward[TLBundleB] {
  def ProbeBlock(testFunc: TestFunc)(param: Permission, size: Int, source: Int, address: Int, mask: Int) = {
    testFunc.execute(this)(edge.flip.ProbeBlock(param, size, source, address, mask))
  }

  def ProbePerm(testFunc: TestFunc)(param: Permission, size: Int, source: Int, address: Int, mask: Int) =
    testFunc.execute(this)(edge.flip.ProbeBlock(param, size, source, address, mask))

  def Get(testFunc: TestFunc)(size: Int, source: Int, address: Int, mask: Int) =
    testFunc.execute(this)(edge.flip.Get(size, source, address, mask))

  def PutFullData(testFunc: TestFunc)(size: Int, source: Int, address: Int, mask: Int, corrupt: Boolean, data: BigInt) =
    testFunc.execute(this)(edge.flip.PutFullData(size, source, address, mask, corrupt, data))
}

case class ClientC(data: DecoupledIO[TLBundleC], edge: TLEdgeOut)(implicit val clock: Clock) extends Outward[TLBundleC] {
  def ProbeAck(testFunc: TestFunc)(param: Permission, size: Int, source: Int, address: Int) =
    testFunc.execute(this)(edge.ProbeAck(param, size, source, address))

  def ProbeAckData(testFunc: TestFunc)(param: Permission, size: Int, source: Int, address: Int, corrupt: Boolean, data: BigInt) =
    testFunc.execute(this)(edge.ProbeAckData(param, size, source, address, corrupt, data))

  def Release(testFunc: TestFunc)(param: Permission, size: Int, source: Int, address: Int) =
    testFunc.execute(this)(edge.Release(param, size, source, address))

  def ReleaseData(testFunc: TestFunc)(param: Permission, size: Int, source: Int, address: Int, data: BigInt) =
    testFunc.execute(this)(edge.ReleaseData(param, size, source, address, data))
}

case class ClientD(data: DecoupledIO[TLBundleD], edge: TLEdgeOut)(implicit val clock: Clock) extends Inward[TLBundleD] {
  def AccessAck(testFunc: TestFunc)(size: Int, source: Int, denied: Boolean) =
    testFunc.execute(this)(edge.flip.AccessAck(size, source, denied))

  def AccessAckData(testFunc: TestFunc)(size: Int, source: Int, denied: Boolean, corrupt: Boolean, data: BigInt) =
    testFunc.execute(this)(edge.flip.AccessAckData(size, source, denied, corrupt, data))

  def HintAck(testFunc: TestFunc)(size: Int, source: Int, denied: Boolean) =
    testFunc.execute(this)(edge.flip.HintAck(size, source, denied))

  def Grant(testFunc: TestFunc)(param: Permission, size: Int, source: Int, sink: Int, denied: Int) =
    testFunc.execute(this)(edge.flip.Grant(param, size, source, sink, denied))

  def GrantData(testFunc: TestFunc)(param: Permission, size: Int, source: Int, sink: Int, denied: Int, corrupt: Boolean, data: BigInt) =
    testFunc.execute(this)(edge.flip.GrantData(param, size, source, sink, denied, corrupt, data))

  def ReleaseAck(testFunc: TestFunc)(size: Int, source: Int) =
    testFunc.execute(this)(edge.flip.ReleaseAck(size, source))
}

case class ClientE(data: DecoupledIO[TLBundleE], edge: TLEdgeOut)(implicit val clock: Clock) extends Outward[TLBundleE] {
  def GrantAck(testFunc: TestFunc)(sink: Int) =
    testFunc.execute(this)(edge.GrantAck(sink))
}




