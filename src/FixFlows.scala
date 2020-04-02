package diplomatictester

import firrtl.Mappers._
import firrtl.Utils._
import firrtl._
import firrtl.ir._
import firrtl.passes._

import scala.collection.mutable

object FixFlows extends Pass {
  def run(c: Circuit): Circuit = {
    def fixStatementFlow(flows: mutable.Map[String, Flow])(statement: Statement): Statement = statement match {
      case Connect(info, loc, expr) =>
        if (getFlow(flows)(loc) == SourceFlow & getFlow(flows)(expr) == SinkFlow) Connect(info, expr, loc) else Connect(info, loc, expr)
      case (s: DefWire) =>
        flows(s.name) = DuplexFlow
        s
      case (s: DefRegister) =>
        flows(s.name) = DuplexFlow
        s
      case (s: DefMemory) =>
        flows(s.name) = SourceFlow
        s
      case (s: WDefInstance) =>
        flows(s.name) = SourceFlow
        s
      case (s: DefNode) =>
        flows(s.name) = SourceFlow
        s
      case s =>
        s.map(fixStatementFlow(flows))
    }

    def getFlow(flows: mutable.Map[String, Flow])(e: Expression): Flow = e match {
      case e: WRef => flows(e.name)
      case e: WSubIndex => getFlow(flows)(e.expr)
      case e: WSubAccess => getFlow(flows)(e.expr)
      case e: WSubField => e.expr.tpe match {
        case t: BundleType =>
          val f = (t.fields find (_.name == e.name)).get
          times(getFlow(flows)(e.expr), f.flip)
      }
      case _ => SourceFlow
    }

    val fixedModule = c.modules.map { module =>
      val flows = mutable.Map[String, Flow]()
      flows ++= (module.ports map (p => p.name -> to_flow(p.direction)))
      val newModule = module.map(fixStatementFlow(flows))
      newModule
    }
    c.copy(modules = fixedModule)
  }
}