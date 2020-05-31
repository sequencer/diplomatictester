package diplomatictester.firrtl

import firrtl.annotations.{ReferenceTarget, SingleTargetAnnotation}

/** Annotation to export IO to Top, driven by external module or testers. */
case class InnerIOAnnotation(target: ReferenceTarget, name: String) extends SingleTargetAnnotation[ReferenceTarget] {
  def duplicate(n: ReferenceTarget) = this.copy(target = n)
}

/** Annotate this IO to connect another annotated IO with [[InnerIOAnnotation]]. */
case class TopIOAnnotation(target: ReferenceTarget, name: String) extends SingleTargetAnnotation[ReferenceTarget] {
  def duplicate(n: ReferenceTarget) = this.copy(target = n)
}