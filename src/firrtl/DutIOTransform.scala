package diplomatictester.firrtl

import firrtl._
import firrtl.annotations._
import firrtl.ir._

case class TargetDUTAnnotation(target: InstanceTarget) extends SingleTargetAnnotation[InstanceTarget] {
  def duplicate(n: InstanceTarget) = this.copy(target = n)
}

class DutIOTransform extends Transform {
  def inputForm: CircuitForm = MidForm

  def outputForm: CircuitForm = MidForm

  def execute(state: CircuitState): CircuitState = {
    val dutInstances = state.annotations.collect { case TargetDUTAnnotation(target) => target }
    require(dutInstances.size == 1, "only 1 instance can be annotate as DUT.")
    val dutInstanceTarget = dutInstances.head
    val dutModuleTarget = dutInstanceTarget.moduleTarget
    val dutModule = state.circuit.modules.find { module => ModuleTarget(state.circuit.main, module.name) == dutModuleTarget }.get
    val topModule = Module(NoInfo, dutModuleTarget.name + "_dut", dutModule.ports, Block(
      DefInstance(NoInfo, dutInstanceTarget.name, dutModule.name) +:
        dutModule.ports.map { case port@Port(_, name, direction, typ) =>
          direction match {
            case Input => Connect(NoInfo, WSubField(WRef(dutInstanceTarget.name), name), WRef(port))
            case Output => Connect(NoInfo, WRef(port), WSubField(WRef(dutInstanceTarget.name), name))
          }
        }
    ))
    state.copy(state.circuit.copy(modules = topModule +: state.circuit.modules, main = dutModuleTarget.name + "_dut"))
  }
}
