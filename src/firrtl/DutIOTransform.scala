package diplomatictester.firrtl

import firrtl._
import firrtl.annotations._
import firrtl.ir._
import firrtl.passes._

class DutIOTransform extends Transform {
  def inputForm: CircuitForm = MidForm

  def outputForm: CircuitForm = MidForm

  def execute(state: CircuitState): CircuitState = {
    val topModuleTarget: ModuleTarget = ModuleTarget(state.circuit.main, state.circuit.main)
    /** a tuple of [[InnerIOAnnotation]], [[TopIOAnnotation]] and io name. */
    val (ioPairs: Map[String, (ReferenceTarget, ReferenceTarget)], dutModule: DefModule, dutModuleTarget: ModuleTarget) = {
      /** get innerIOs. */
      val innerIOs = state.annotations.collect { case InnerIOAnnotation(target, name) => target -> name }
      innerIOs.groupBy(_._2).mapValues(_.size).foreach { case (name, num) =>
        require(num == 1, s"InnerIOAnnotation(someIO, $name) has appeared $num")
      }
      innerIOs.foreach {
        case (rt, _) => require(rt.moduleTarget != topModuleTarget, s"InnerIOAnnotation cannot appear in Top Module.")
      }
      require(innerIOs.map(io => io._1.moduleTarget).toSet.size == 1, s"only one module could be annotated to DUT, annotated: ${
        innerIOs.map(io => io._1.moduleTarget.name).toSet.reduceLeft(_ + ", " + _)
      }")
      val dutModuleTarget = innerIOs.head._1.moduleTarget
      val dutModule = state.circuit.modules.find { module => ModuleTarget(state.circuit.main, module.name) == dutModuleTarget }.get
      val dutModulePortTarget = dutModule.ports.map(p => Target.asTarget(dutModuleTarget)(WRef(p)))
      require(dutModulePortTarget.toSet == innerIOs.map(_._1).toSet, s"IO of Module ${dutModuleTarget.name} is not fully annotated, remain: ${
        (dutModulePortTarget.toSet diff innerIOs.map(_._1).toSet).map(_.name).reduceLeft(_ + ", " + _)
      }.")
      /** get topIOs. */
      val topIOs = state.annotations.collect { case TopIOAnnotation(target, name) => name -> target }
      topIOs.groupBy(_._1).mapValues(_.size).foreach { case (name, num) =>
        require(num == 1, s"TopIOAnnotation(someIO, $name) has appeared $num")
      }
      topIOs.foreach {
        case (_, rt) => require(rt.moduleTarget == topModuleTarget, s"TopIOAnnotation only can appear in Top Module.")
      }
      val innerIONames = innerIOs.map(_._2)
      val topIONames = topIOs.map(_._1)
      val innerDiffTop = innerIONames diff topIONames
      val topDiffInner = topIONames diff innerIONames
      innerDiffTop.foreach { name => throw new Exception(s"TopIOAnnotation(someIO, $name) doesn't appear in InnerIOAnnotation(someIO, $name)") }
      topDiffInner.foreach { name => throw new Exception(s"InnerIOAnnotation(someIO, $name) doesn't appear in TopIOAnnotation(someIO, $name)") }
      val topIOMap = topIOs.toMap
      (innerIOs.map { case (t, n) =>
        t.name -> (t, topIOMap(n))
      }.toMap, dutModule, dutModuleTarget)
    }
    val dutInstanceName = dutModule.name + "_dut"
    val ioInfo: Map[String, (Type, Direction)] = dutModule.ports.flatMap {
      p: Port =>
        val rt = Target.asTarget(ModuleTarget(state.circuit.main, dutModule.name))(WRef(p))
        ioPairs.find(_._2._1 == rt) match {
          case None => Nil
          case Some(pair) => Map(pair._1 -> (p.tpe, p.direction))
        }
    }.toMap
    /** use annotated val name, not io name in dut. */
    val topPorts = ioPairs.map(pair => pair._1 -> Port(NoInfo, pair._2._2.name, ioInfo(pair._1)._2, ioInfo(pair._1)._1)).toMap
    val dutInstance = DefInstance(NoInfo, dutInstanceName, dutModule.name)
    val connects = ioPairs.map(pair => Connect(NoInfo, WSubField(WRef(dutInstanceName), pair._1), WRef(topPorts(pair._1)))).toSeq
    /** #1513 */
    val topModule = Module(NoInfo, state.circuit.main, topPorts.values.toArray.toSeq,
      Block(dutInstance +: connects))
    val annosx = state.annotations.filter {
      case _: InnerIOAnnotation => false
      case _: TopIOAnnotation => false
      case _ => true
    }
    val newCircuit = state.circuit.copy(modules = state.circuit.modules.filterNot(_.name == state.circuit.main) :+ topModule)
    val fixedCircuit = fixupCircuit(newCircuit)
    state.copy(fixedCircuit, annotations = annosx)
  }

  /** Run passes to fix up the circuit of making the new connections  */
  private def fixupCircuit(circuit: Circuit): Circuit = {
    val passes = Seq(
      ToWorkingIR,
      InferTypes,
      ResolveKinds,
      ResolveFlows,
      ExpandConnects,
      FixFlows,
      InferTypes,
      ResolveKinds,
      ResolveFlows,
      CheckFlows
    )
    passes.foldLeft(circuit) { case (c: Circuit, p: Pass) => p.run(c) }
  }
}
