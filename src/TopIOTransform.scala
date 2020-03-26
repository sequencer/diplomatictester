package diplomatictester

import firrtl._
import firrtl.annotations._
import firrtl.ir.Circuit
import firrtl.passes.Pass
import firrtl.passes.wiring.Modifications

import scala.collection.mutable

/** Annotation to export IO to Top, driven by external module or testers. */
case class InnerIOAnnotation(target: ReferenceTarget, name: String) extends SingleTargetAnnotation[ReferenceTarget] {
  def duplicate(n: ReferenceTarget) = this.copy(target = n)
}

/** Annotate this IO to connect another annoated IO with [[InnerIOAnnotation]]. */
case class TopIOAnnotation(target: ReferenceTarget, name: String) extends SingleTargetAnnotation[ReferenceTarget] {
  def duplicate(n: ReferenceTarget) = this.copy(target = n)
}

case class BoringPair(name: String, topIO: ReferenceTarget, innerIO: ReferenceTarget) {
  require(topIO.path.isEmpty, s"${topIO.name} is not at top")

  /** Get a instance sequence from inner to top. */
  def path = innerIO.path
}

class TopIOTransform extends Transform {
  def inputForm: CircuitForm = MidForm

  def outputForm: CircuitForm = HighForm

  def execute(state: CircuitState): CircuitState = {
    /** Get annotations. */
    val innerIO: Map[String, ReferenceTarget] = state.annotations.collect {
      case InnerIOAnnotation(t, n) => (n -> t)
    }.toMap
    val topIO: Map[String, ReferenceTarget] = state.annotations.collect {
      case TopIOAnnotation(t, n) => (n -> t)
    }.toMap

    /** assert topIO is at top. */
    topIO.foreach(_._2.path.isEmpty)
    /** Get inner target to top target map. */
    val boringPairs = topIO.map {
      case (n, t) => BoringPair(n, t, innerIO(n))
    }.toSeq

    /** Get [[Modifications]] introduced by [[boringPairs]] */
    val modifications: Seq[Seq[(ModuleTarget, Modifications)]] = boringPairs.map(computeModifications)

    /** Change each modules. */
    state.copy(circuit = modifications.foldLeft(state.circuit) {
      case (c, m) => new Modify(m).run(c)
    })
  }

  private def computeModifications(boringPair: BoringPair): Seq[(ModuleTarget, Modifications)] = {
    val meta = new mutable.HashMap[ModuleTarget, Modifications].withDefaultValue(Modifications())
    val paths = boringPair.path
    ???
    meta.toSeq
  }

}

class Modify(modifications: Seq[(ModuleTarget, Modifications)]) extends Pass {
  override def run(c: Circuit): Circuit = ???
}