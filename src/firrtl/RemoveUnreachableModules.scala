package diplomatictester.firrtl

import firrtl._
import firrtl.annotations.NoTargetAnnotation
import firrtl.options._

class RemoveUnreachableModules extends Transform with PreservesAll[Transform] {
  override def inputForm: CircuitForm = UnknownForm

  override def outputForm: CircuitForm = UnknownForm

  override val prerequisites = firrtl.stage.Forms.MinimalHighForm

  def execute(state: CircuitState): CircuitState = {
    val c = state.circuit
    val annosx = state.annotations
    val newCircuit = c.copy(modules = c.modules.filter(module => new firrtl.analyses.InstanceGraph(c).reachableModules.map(_.value).contains(module.name)))
    val targetAnnosx = annosx.filterNot(_.isInstanceOf[NoTargetAnnotation])
    targetAnnosx.foreach(pprint.pprintln(_))
    state.copy(circuit = newCircuit)
  }
}
