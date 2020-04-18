package diplomatictester.firrtl

import firrtl._
import firrtl.options._
import firrtl.stage.TransformManager

// @todo upstream this
trait TransformBatch extends Transform with PreservesAll[Transform] {
  def inputForm: CircuitForm = UnknownForm

  def outputForm: CircuitForm = UnknownForm

  implicit class TransformPatch(transform: Transform) {
    def recursivePrerequisites: Seq[Dependency[Transform]] = transform.prerequisites.flatMap(_.getObject().prerequisites.flatMap(_.getObject().recursivePrerequisites)) ++ transform.prerequisites
  }

  def transforms: Seq[Transform]

  final override def prerequisites: Seq[Dependency[Transform]] = transforms.flatMap(_.recursivePrerequisites).distinct

  final def allTransforms: Seq[Dependency[Transform]] = (transforms.map(t => Dependency.fromTransform(t)) ++
    prerequisites.filter { prerequisite =>
      transforms.map {
        transform =>
          transform.invalidates(prerequisite.getObject())
      }.reduce(_ | _)
    } ++
    transforms.flatMap {
      _.dependents.flatMap(_.getObject().recursivePrerequisites)
    }
    ).distinct


  protected def runTransforms(state: CircuitState): CircuitState = new TransformManager(allTransforms, prerequisites).flattenedTransformOrder.foldLeft(state) { (in, xform) => xform.runTransform(in) }

  def execute(state: CircuitState): CircuitState = {
    val ret = runTransforms(state)
    CircuitState(ret.circuit, outputForm, ret.annotations, ret.renames)
  }
}