package diplomatictester

import firrtl._
import firrtl.analyses.InstanceGraph
import firrtl.annotations._
import firrtl.ir._
import firrtl.passes._

import scala.collection.mutable

/** Annotation to export IO to Top, driven by external module or testers. */
case class InnerIOAnnotation(target: ReferenceTarget, name: String) extends SingleTargetAnnotation[ReferenceTarget] {
  def duplicate(n: ReferenceTarget) = this.copy(target = n)
}

/** Annotate this IO to connect another annotated IO with [[InnerIOAnnotation]]. */
case class TopIOAnnotation(target: ReferenceTarget, name: String) extends SingleTargetAnnotation[ReferenceTarget] {
  def duplicate(n: ReferenceTarget) = this.copy(target = n)
}

/** firrtl need to decide:
  * How [[TopIOTransform]] is injected to all Transforms
  * [[inputForm]] and [[outputForm]] to decide the location of transform injection.
  * and is deprecated after Dependency API added
  * */
class TopIOTransform extends Transform {
  def inputForm: CircuitForm = MidForm

  def outputForm: CircuitForm = MidForm

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
        instanceGraph.findInstancesInHierarchy(t.moduleTarget.module).head.foldLeft("_TOPIO_")(_ + _.name + "_") + t.name -> (t, topIOMap(n))
      }
      }.toMap
    val bottomModels: Seq[ModuleTarget] = ioPairs.values.map(_._1.moduleTarget).toSeq
    /** collect port inner Module dir and type information from entire circuit. */
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

    def resolveFlow(name: String) = ioInfo(name)._2 match {
      case Input => SourceFlow
      case Output => SinkFlow
    }

    def flipDirection(dir: Direction) = dir match {
      case Input => Output
      case Output => Input
    }

    def flipFlow(flow: Flow) = flow match {
      case SourceFlow => SinkFlow
      case SinkFlow => SourceFlow
    }

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

    val moduleInstanceMap: Map[ModuleTarget, Seq[InstanceTarget]] = {
      moduleNewPortsMap.keys.map {
        module => module -> instanceGraph.getChildrenInstances(module.module).map(wdi => module.instOf(wdi.name, wdi.module)).toSeq
      }.toMap
    }

    val updatedModules: Seq[DefModule] = state.circuit.modules.map {
      module =>

        /** [[ModuleTarget]] based on [[DefModule]]. */
        val moduleTarget: ModuleTarget = ModuleTarget(state.circuit.main, module.name)
        if (moduleNewPortsMap.contains(moduleTarget)) {
          /** bottom module */
          if (bottomModels.contains(moduleTarget)) {
            /** require all output IO is annotated, since whole module will be replaced. */
            val modulePorts = module.ports.map(p => Target.asTarget(moduleTarget)(WRef(p)) -> p).toMap
            val annotatedPorts = ioPairs.filter(p => modulePorts.keys.toSeq.contains(p._2._1)).map { case (name, (source, _)) => name -> source }
            require((modulePorts.filter(_._2.direction == Output).keys.toSeq diff annotatedPorts.values.toSeq).isEmpty, s"IO of bottom Moudle ${moduleTarget.name} are not fully annotated.")
            val blocks = annotatedPorts.toSeq.map { case (name, rt) =>
              val p = modulePorts(rt)
              val pair = ioPairs.find(_._2._1 == rt).get
              val newPort = Port(NoInfo, pair._1, flipDirection(ioInfo(name)._2), ioInfo(name)._1)
              val newConnect = Connect(NoInfo, WRef(newPort).copy(flow = resolveFlow(name)), WRef(p).copy(flow = flipFlow(resolveFlow(name))))
              (newPort, newConnect)
            }
            Module(module.info, module.name, module.ports ++ blocks.map(_._1), Block(blocks.map(_._2)))
          }

          /** top module
            * connect [[TopIOAnnotation]] annotated [[ReferenceTarget]] to instance where have auto generated IO
            * */
          else if (moduleTarget == topModel) {
            /** all Module IO Refs. */
            val ioRefMap: Map[String, WRef] = moduleNewPortsMap(moduleTarget).map { name =>
              name -> WRef(Port(NoInfo, ioPairs(name)._2.ref, ioInfo(name)._2, ioInfo(name)._1))
            }.toMap
            /** get all sub-Module of this Module. */
            val subModules = moduleInstanceMap(moduleTarget)
            /** all Ref to new ports. */
            val instanceNewPorts: Map[String, WSubField] = subModules.flatMap { it =>
              moduleNewPortsMap(it.moduleTarget).map(portName => portName -> WSubField(WRef(it.name), portName))
            }.toMap
            /** generate new connections. */
            val netConnections = ioRefMap.map { case (name, port) => Connect(NoInfo, port.copy(flow = flipFlow(resolveFlow(name))), instanceNewPorts(name).copy(flow = resolveFlow(name))) }.toSeq
            val m = module.asInstanceOf[Module]
            m.copy(body = Block(m.body +: netConnections))
          }

          /** middle module */
          else {
            /** all Module IO Refs. */
            val newPorts: Map[String, Port] = moduleNewPortsMap(moduleTarget).map { name =>
              name -> Port(NoInfo, name, flipDirection(ioInfo(name)._2), ioInfo(name)._1)
            }.toMap
            /** get all sub-Module of this Module. */
            val subModules = moduleInstanceMap(moduleTarget)
            /** all Ref to new ports. */
            val instanceNewPorts: Map[String, WSubField] = subModules.flatMap { it =>
              moduleNewPortsMap.get(it.ofModuleTarget) match {
                case Some(ports) => ports.map(portName => portName -> WSubField(WRef(it.name), portName))
                case None => Nil
              }
            }.toMap
            val netConnections = newPorts.map { case (name, port) => Connect(NoInfo, instanceNewPorts(name).copy(flow = flipFlow(resolveFlow(name))), WRef(port).copy(flow = resolveFlow(name))) }.toSeq
            val m = module.asInstanceOf[Module]
            m.copy(ports = m.ports ++ newPorts.values, body = Block(m.body +: netConnections))
          }
        }

        /** other module */
        else module
    }
    val newCircuit = state.circuit.copy(modules = updatedModules)
    val fixedCircuit = fixupCircuit(newCircuit)
    val annosx = state.annotations.filter {
      case _: InnerIOAnnotation => false
      case _: TopIOAnnotation => false
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
      ResolveFlows,
      FixFlows,
      CheckFlows
    )
    passes.foldLeft(circuit) { case (c: Circuit, p: Pass) => p.run(c) }
  }
}