package diplomatictester

import firrtl._
import firrtl.analyses.InstanceGraph
import firrtl.annotations._
import firrtl.ir._
import firrtl.passes._
import pprint.pprintln
import scala.collection.mutable

/** Annotation to export IO to Top, driven by external module or testers. */
case class InnerIOAnnotation(target: ReferenceTarget, name: String) extends SingleTargetAnnotation[ReferenceTarget] {
  def duplicate(n: ReferenceTarget) = this.copy(target = n)
}

/** Annotate this IO to connect another annotated IO with [[InnerIOAnnotation]]. */
case class TopIOAnnotation(target: ReferenceTarget, name: String) extends SingleTargetAnnotation[ReferenceTarget] {
  def duplicate(n: ReferenceTarget) = this.copy(target = n)
}

class TopIOTransform extends Transform {
  def inputForm: CircuitForm = MidForm

  def outputForm: CircuitForm = MidForm

  type InstPath = Seq[String]

  def execute(state: CircuitState): CircuitState = {
    /** [[InstanceGraph]] to generate */
    val instanceGraph: InstanceGraph = new firrtl.analyses.InstanceGraph(state.circuit)
    /** TopModule Target. */
    val topModel: ModuleTarget = ModuleTarget(state.circuit.main, state.circuit.main)
    /** a tuple of [[InnerIOAnnotation]], [[TopIOAnnotation]] and io name. */
    val ioPairs: Map[String, (ReferenceTarget, ReferenceTarget)] = {
      /** get innerIOs. */
      val innerIOs = state.annotations.collect { case InnerIOAnnotation(target, name) => target -> name }
      innerIOs.groupBy(_._2).mapValues(_.size).foreach { case (name, num) =>
        require(num == 1, s"InnerIOAnnotation(someIO, $name) has appeared $num")
      }
      innerIOs.foreach {
        case (rt, _) => require(rt.moduleTarget != topModel, s"InnerIOAnnotation cannot appear in Top Module.")
      }
      /** get topIOs. */
      val topIOs = state.annotations.collect { case TopIOAnnotation(target, name) => name -> target }
      topIOs.groupBy(_._1).mapValues(_.size).foreach { case (name, num) =>
        require(num == 1, s"TopIOAnnotation(someIO, $name) has appeared $num")
      }
      topIOs.foreach {
        case (_, rt) => require(rt.moduleTarget == topModel, s"TopIOAnnotation only can appear in Top Module.")
      }
      val innerIONames = innerIOs.map(_._2)
      val topIONames = topIOs.map(_._1)
      val innerDiffTop = innerIONames diff topIONames
      val topDiffInner = topIONames diff innerIONames
      innerDiffTop.foreach { name => throw new Exception(s"TopIOAnnotation(someIO, $name) doesn't appear in InnerIOAnnotation(someIO, $name)") }
      topDiffInner.foreach { name => throw new Exception(s"InnerIOAnnotation(someIO, $name) doesn't appear in TopIOAnnotation(someIO, $name)") }
      val topIOMap = topIOs.toMap
      innerIOs.map { case (t, n) =>
        instanceGraph.findInstancesInHierarchy(t.moduleTarget.module).head.foldLeft("_TOPIO")(_ + "_" + _.name) + t.name -> (t, topIOMap(n))
      }
      }.toMap
    val bottomModels: Seq[ModuleTarget] = ioPairs.values.map(_._2.moduleTarget).toSeq
    /** collect port information from entire circuit. */
    val ioInfo: Map[String, (Type, Direction)] = state.circuit.modules.flatMap { m =>
      m.ports.flatMap {
        p: Port =>
          val rt = Target.asTarget(ModuleTarget(state.circuit.main, m.name))(WRef(p))
          ioPairs.find(_._2._1 == rt) match {
            case None => Nil
            case Some(pair) => Map(pair._1 -> (p.tpe, p.direction))
          }
      }
    }.toMap
    /** each module on the boring path will add new ports */
    val moduleNewPortsMap: Map[ModuleTarget, Seq[String]] = {
      val m = mutable.Map[ModuleTarget, Seq[String]]()
      ioPairs.foreach {
        case (name, (rt, _)) =>

          /** update mutable map */
          instanceGraph.findInstancesInHierarchy(rt.moduleTarget.module).head.map(wdi => ModuleTarget(state.circuit.main, wdi.module)).foreach { mt =>
            m.get(mt) match {
              case Some(v) => m(mt) = v :+ name
              case None => m(mt) = Seq(name)
            }
          }
      }
      m.toMap
    }
    val updatedModules: Seq[DefModule] = state.circuit.modules.map {
      module =>

        /** [[ModuleTarget]] based on [[DefModule]]. */
        val moduleTarget: ModuleTarget = ModuleTarget(state.circuit.main, module.name)
        /** current module IO. */
        val portTargets = module.ports.map(p => Target.asTarget(moduleTarget)(WRef(p)))
        if (moduleNewPortsMap.contains(moduleTarget)) {

          /** bottom module */
          if (bottomModels.contains(moduleTarget)) {
            /** require all IO is annotated, since whole module will be replaced. */
            val portTargets = module.ports.map(p => Target.asTarget(moduleTarget)(WRef(p)) -> p).toMap
            require((portTargets.keys.toSeq diff ioPairs.map(_._2._1).toSeq).isEmpty, s"IO of bottom Moudle ${moduleTarget.name} are not fully annotated.")
            val blocks = portTargets.map { case (rt, p) =>
              val pair = ioPairs.find(_._2._1 == rt).get
              val newPort = Port(NoInfo, pair._1, p.direction match {
                case Input => Output
                case Output => Input
              }, p.tpe)
              val newConnect = Connect(NoInfo, WRef(newPort), WRef(p))
              (newPort, newConnect)
            }.toSeq
            Module(module.info, module.name, module.ports ++ blocks.map(_._1), Block(blocks.map(_._2)))
          }

          /** top module */
          else if (moduleTarget == topModel) {
            /** all annotated Top IOs. */
            val portTargets = moduleNewPortsMap(moduleTarget).map(ioPairs).map(_._2)
            /** get all reference target of port. */

          }

          /** middle module */
          else {
            /** new IOs. */
            val newPorts = moduleNewPortsMap(moduleTarget).map(name => Port(NoInfo, name, ioInfo(name)._2, ioInfo(name)._1))
            val portTargets = newPorts.map(p => Target.asTarget(moduleTarget)(WRef(p)) -> p).toMap
            /** generate new block */
            val newConnects = portTargets.map {
              case (rt, p) =>

                /** Filter sub-instance which contains this new IO. */
                Connect(NoInfo, WSubField(WRef(???), p.name), WRef(p))
            }
          }

        }

        /** other module */
        else module
    }
    val newCircuit = state.circuit.copy(modules = updatedModules)
    val fixedCircuit = fixupCircuit(newCircuit)
    val annosx = state.annotations.filter {
      case _: InnerIOAnnotation => false
      case _ => true
    }
    state.copy(circuit = fixedCircuit, annotations = annosx)
  }

  /** Run passes to fix up the circuit of making the new connections  */
  private def fixupCircuit(circuit: Circuit): Circuit = {
    val passes = Seq(
      InferTypes,
      ResolveKinds,
      ResolveFlows,
      ExpandConnects,
      InferTypes,
      ResolveKinds,
      ResolveFlows
    )
    passes.foldLeft(circuit) { case (c: Circuit, p: Pass) => p.run(c) }
  }
}
