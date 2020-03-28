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
    val sources = getSourcesMap(state)
    val portnamesmap: mutable.Map[String, String] = mutable.Map()
    val instanceGraph = new firrtl.analyses.InstanceGraph(state.circuit)
    val namespaceMap = state.circuit.modules.map(m => m.name -> Namespace(m)).toMap
    val modulesx = state.circuit.modules map onModule(state, sources, portnamesmap, instanceGraph, namespaceMap)
    val newCircuit = state.circuit.copy(modules = modulesx)
    val fixedCircuit = fixupCircuit(newCircuit)
    val annosx = state.annotations.filter {
      case _: InnerIOAnnotation => false
      case _ => true
    }
    state.copy(circuit = fixedCircuit, annotations = annosx)
  }

  /** Get the names of the targets that need to be wired */
  private def getSourceNames(state: CircuitState): Map[ReferenceTarget, String] = {
    state.annotations.collect { case InnerIOAnnotation(srcname, name) => srcname -> name
    }.toMap.withDefaultValue("")
  }

  /** Get the names of the modules which include the  targets that need to be wired */
  private def getSourceModNames(state: CircuitState): Seq[CompleteTarget] = {
    state.annotations.collect { case InnerIOAnnotation(t, _) => t.targetParent }
  }

  /** Get the Type of each port to be connected
    *
    * Similar to getSourceTypes, but specifically for ports since they are not found in statements.
    * Find the definition of each port in sourceList, and get the type and whether or not it's a port
    * Update the results in sourceMap
    */
  private def getSourceTypesPorts(sourceList: Map[ReferenceTarget, String],
                                  sourceMap: mutable.Map[CompleteTarget, Seq[(ReferenceTarget, Type, InstPath, String)]],
                                  currentmodule: ModuleTarget,
                                  state: CircuitState)(s: Port): CircuitState = s match {
    // If target port, add name and size to to sourceMap
    case w: IsDeclaration =>
      if (sourceList.keys.toSeq.contains(currentmodule.ref(w.name))) {
        val (tpe, prefix) = w match {
          case d: Port => (d.tpe, sourceList(currentmodule.ref(w.name)))
          case _ => throw new Exception(s"Cannot wire this type of declaration! ${w.serialize}")
        }
        sourceMap.get(currentmodule) match {
          case Some(xs: Seq[(ReferenceTarget, Type, InstPath, String)]) =>
            sourceMap.update(currentmodule, xs :+ (
              (currentmodule.ref(w.name), tpe, Seq[String](w.name), prefix)))
          case None =>
            sourceMap(currentmodule) = Seq((currentmodule.ref(w.name), tpe, Seq[String](w.name), prefix))
        }
      }
      state // Return argument unchanged (ok because DefWire has no Statement children)
    // If not, apply to all children Statement
    case _ => state
  }


  /** Create a map of Module name to target wires under this module
    * These paths are relative but cross module (they refer down through instance hierarchy)
    */
  private def getSourcesMap(state: CircuitState): Map[CompleteTarget, Seq[(ReferenceTarget, Type, InstPath, String)]] = {
    /** modules where contains the [[InnerIOAnnotation]]. */
    val sourceModules = getSourceModNames(state)
    /** All the sources to be wired. */
    val portsToBeWired = getSourceNames(state)
    /** analyses the instance graph. */
    val instGraph: InstanceGraph = new firrtl.analyses.InstanceGraph(state.circuit)
    /** a map of Instance -> (innerInstance, innerModule).
      *
      * @todo use [[Target]]
      * */
    val moduleHierarchyMap: Map[ModuleTarget, Seq[InstanceTarget]] = instGraph.getChildrenInstances.map { case (m, wdis) =>
      val moduleTarget = ModuleTarget(state.circuit.main, m)
      moduleTarget -> wdis.map(wdi => InstanceTarget(state.circuit.main, wdi.module, moduleTarget.path, wdi.name, wdi.module)).toSeq
    }.toMap

    /** a sorted Module */
    val topSortedModules: Seq[ModuleTarget] = instGraph.moduleOrder.reverse.map(m => ModuleTarget(state.circuit.main, m.name))

    /** Map of component name to relative instance paths that result in a debug wire.  */
    val sourceModulesPortsDebug: mutable.Map[CompleteTarget, Seq[(ReferenceTarget, Type, InstPath, String)]] =
      mutable.Map(sourceModules.map(_ -> Seq()): _*)

    state.circuit.modules.foreach { m =>
      m.ports.foreach {
        p =>
          getSourceTypesPorts(portsToBeWired, sourceModulesPortsDebug, ModuleTarget(state.circuit.main, m.name), state)(p)
      }
    }
    topSortedModules.foreach { mod =>
      val seqChildren: Seq[(ReferenceTarget, Type, InstPath, String)] = moduleHierarchyMap(mod).flatMap(inst =>
        sourceModulesPortsDebug.get(inst.ofModuleTarget).map(_.map { case (a, b, path, p) => (a, b, inst.name +: path, p) })).flatten
      if (seqChildren.nonEmpty) {
        sourceModulesPortsDebug(mod) = sourceModulesPortsDebug.getOrElse(mod, Seq()) ++ seqChildren
      }
    }
    sourceModulesPortsDebug.toMap
  }


  /** Process a given DefModule
    *
    * For Modules that contain or are in the parent hierarchy to modules containing target wires
    * 1. Add ports for each target wire this module is parent to
    * 2. Connect these ports to ports of instances that are parents to some number of target wires
    */
  private def onModule(state: CircuitState,
                       sources: Map[CompleteTarget, Seq[(ReferenceTarget, Type, InstPath, String)]],
                       portnamesmap: mutable.Map[String, String],
                       instgraph: firrtl.analyses.InstanceGraph,
                       namespacemap: Map[String, Namespace])
                      (module: DefModule): DefModule = {
    val namespace = namespacemap(module.name)
    val moduleTarget = ModuleTarget(state.circuit.main, module.name)
    /** rewrite here,
      * 1. flip IO and connect
      * 2. at top module, assign IO to [[TopIOAnnotation]]
      * */
    sources.get(moduleTarget) match {
      case Some(p) =>
        val newPorts = p.map {
          case (t, tpe, path, prefix) =>
            val newportname = portnamesmap.get(prefix + path.mkString("_")) match {
              case Some(pn) => pn
              case None => {
                val npn = namespace.newName(prefix + path.mkString("_"))
                portnamesmap(prefix + path.mkString("_")) = npn
                npn
              }
            }
            Port(NoInfo, newportname, Output, tpe)
        }

        // Add connections to Module
        module match {
          case m: Module =>
            val connections: Seq[Connect] = p.map { case (t, _, path, prefix) =>
              val modRef = portnamesmap.get(prefix + path.mkString("_")) match {
                case Some(pn) => WRef(pn)
                case None => {
                  portnamesmap(prefix + path.mkString("_")) = namespace.newName(prefix + path.mkString("_"))
                  WRef(portnamesmap(prefix + path.mkString("_")))
                }
              }
              path.size match {
                case 1 => {
                  val leafRef = WRef(path.head.mkString(""))
                  Connect(NoInfo, modRef, leafRef)
                }
                case _ => {
                  val instportname = portnamesmap.get(prefix + path.tail.mkString("_")) match {
                    case Some(ipn) => ipn
                    case None => {
                      val instmod = instgraph.getChildrenInstances(module.name).collectFirst {
                        case wdi if wdi.name == path.head => wdi.module
                      }.get
                      val instnamespace = namespacemap(instmod)
                      portnamesmap(prefix + path.tail.mkString("_")) =
                        instnamespace.newName(prefix + path.tail.mkString("_"))
                      portnamesmap(prefix + path.tail.mkString("_"))
                    }
                  }
                  val instRef = WSubField(WRef(path.head), instportname)
                  Connect(NoInfo, modRef, instRef)
                }
              }
            }
            m.copy(ports = m.ports ++ newPorts, body = Block(Seq(m.body) ++ connections))
          case e: ExtModule =>
            e.copy(ports = e.ports ++ newPorts)
        }
      case None => module // unchanged if no paths
    }
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
