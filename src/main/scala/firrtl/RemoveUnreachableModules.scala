package diplomatictester.firrtl

import firrtl._
import firrtl.options._
import firrtl.transforms.DontTouchAnnotation

class RemoveUnreachableModules extends Transform with DependencyAPIMigration {
  override val prerequisites = firrtl.stage.Forms.MinimalHighForm

  override def invalidates(a: Transform): Boolean = false

  def execute(state: CircuitState): CircuitState = {
    val circuit = state.circuit
    val annotations = state.annotations
    val newModules = circuit.modules.filter(module => new firrtl.analyses.InstanceGraph(circuit).reachableModules.map(_.value).contains(module.name))
    val newAnnotations = annotations.filter {
      case DontTouchAnnotation(target) => if (newModules.map(_.name).contains(target.name)) true else false
      case _ => true
    }
    val newCircuit = circuit.copy(modules = newModules)
    state.copy(circuit = newCircuit, annotations = newAnnotations)
  }
}
