package diplomatictester

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
}
