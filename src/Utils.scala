package diplomatictester

import chisel3._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

object Utils {

  implicit class TLEdgeInHelper[T <: TLEdgeIn](edge: T) {
    def flip = new TLEdgeOut(edge.client, edge.manager, edge.params, edge.sourceInfo)
  }

  implicit class TLEdgeOutHelper[T <: TLEdgeOut](edge: T) {
    def flip = new TLEdgeIn(edge.client, edge.manager, edge.params, edge.sourceInfo)
  }

  implicit class LazyModuleHelper[T <: LazyModule](lm: T) {
    def childrenFinder(filter: LazyModule => Boolean): LazyModule = {
      val found = scala.collection.mutable.ListBuffer[LazyModule]()
      lm.childrenIterator(lm => if (filter(lm)) found += lm)
      require(found.nonEmpty, s"no LazyModule found")
      require(found.size == 1, s"constraint of filter is too loose, found ${found.size} LazyModules")
      found.head
    }

    def nodeFinder(filter: BaseNode => Boolean): BaseNode = {
      val found = scala.collection.mutable.ListBuffer[BaseNode]()
      lm.nodeIterator(node => if (filter(node)) found += node)
      require(found.nonEmpty, s"no BaseNode found")
      require(found.size == 1, s"constraint of filter is too loose, found ${found.size} LazyModules")
      found.head
    }
  }

  implicit class AutoBundleToTLBundle[T <: AutoBundle](data: T) {
    def tlBundle(element: String): TLBundle = {
      data.elements(element).asInstanceOf[TLBundle]
    }
  }

  implicit class TLBundleHelper(data: TLBundle)(implicit clock: Clock) {
    def clientA(edge: TLEdgeIn) = ClientA(data.a, edge.flip)

    def clientB(edge: TLEdgeIn) = ClientB(data.b, edge.flip)

    def clientC(edge: TLEdgeIn) = ClientC(data.c, edge.flip)

    def clientD(edge: TLEdgeIn) = ClientD(data.d, edge.flip)

    def clientE(edge: TLEdgeIn) = ClientE(data.e, edge.flip)

    def managerA(edge: TLEdgeOut) = ManagerA(data.a, edge.flip)

    def managerB(edge: TLEdgeOut) = ManagerB(data.b, edge.flip)

    def managerC(edge: TLEdgeOut) = ManagerC(data.c, edge.flip)

    def managerD(edge: TLEdgeOut) = ManagerD(data.d, edge.flip)

    def managerE(edge: TLEdgeOut) = ManagerE(data.e, edge.flip)
  }

}
