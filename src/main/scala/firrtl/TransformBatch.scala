package diplomatictester.firrtl

import firrtl._
import firrtl.options._
import firrtl.stage.TransformManager

/**
 * [[Transform.prerequisites]] will be automatically calculated based on [[Transform.recursivePrerequisites]],
 * which means all [[Transform.prerequisites]] in `targets` will be appended to [[TransformBatch]].
 *
 * [[Transform.optionalPrerequisites]] should be defined by user. this value is locally used by [[TransformManager]],
 * which means all [[Transform.optionalPrerequisites]] in `targets` Transform won't affect external [[TransformManager]].
 *
 * [[Transform.optionalPrerequisiteOf]] should be defined by user. this value is locally used by [[TransformManager]],
 * which means all [[Transform.optionalPrerequisiteOf]] in `targets` Transform won't affect external [[TransformManager]].
 *
 * [[Transform.invalidates]] should be defined by user. this value is locally used by [[TransformManager]],
 * which means all [[Transform.invalidates]] in `targets` Transform won't affect external [[TransformManager]].
 **/
trait TransformBatch extends Transform with DependencyAPIMigration {

  implicit class TransformPatch(transform: Transform) {
    /** get all prerequisites recursively. */
    def recursivePrerequisites: Seq[Dependency[Transform]] = transform.prerequisites.flatMap(_.getObject().prerequisites.flatMap(_.getObject().recursivePrerequisites)) ++ transform.prerequisites
  }

  /** [[Transform]] need to run. */
  def transforms: Seq[Transform]

  /** prerequisites of [[TransformBatch]]. */
  def batchPrerequisites: Seq[Dependency[Transform]] = Seq.empty

  /** all prerequisites and [[batchPrerequisites]] should be run previously. */
  final override def prerequisites: Seq[Dependency[Transform]] = transforms.flatMap(_.recursivePrerequisites).distinct ++ batchPrerequisites

  /** all Transforms should be run in this [[TransformBatch]]. */
  final def targets: Seq[Dependency[Transform]] = transforms.map(t => Dependency.fromTransform(t)).distinct

  protected def runTransforms(state: CircuitState): CircuitState = new TransformManager(targets, prerequisites).flattenedTransformOrder.foldLeft(state) { (in, xform) => xform.runTransform(in) }

  def execute(state: CircuitState): CircuitState = {
    val ret = runTransforms(state)
    CircuitState(ret.circuit, UnknownForm, ret.annotations, ret.renames)
  }
}