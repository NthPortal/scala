package scala.build

import sbt._, Keys._
import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.plugin.MimaPlugin, MimaPlugin.autoImport._

object MimaFilters extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    val mimaReferenceVersion = settingKey[Option[String]]("Scala version number to run MiMa against")
  }
  import autoImport._

  override val globalSettings = Seq(
    mimaReferenceVersion := Some("2.13.4"),
  )

  val mimaFilters: Seq[ProblemFilter] = Seq[ProblemFilter](
    // KEEP: we don't the reflect internal API isn't public API
    ProblemFilters.exclude[Problem]("scala.reflect.internal.*"),

    // KEEP: java.util.Enumeration.asIterator only exists in later JDK versions (11 at least).  If you build
    // with JDK 11 and run MiMa it'll complain IteratorWrapper isn't forwards compatible with 2.13.0 - but we
    // don't publish the artifact built with JDK 11 anyways
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.convert.JavaCollectionWrappers#IteratorWrapper.asIterator"),

    // Fixes for scala/bug#12009
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.mutable.ArrayBufferView.this"),
    ProblemFilters.exclude[FinalClassProblem]("scala.collection.IndexedSeqView$IndexedSeqViewIterator"),
    ProblemFilters.exclude[FinalClassProblem]("scala.collection.IndexedSeqView$IndexedSeqViewReverseIterator"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView$CheckedIterator"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView$CheckedReverseIterator"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView$Id"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView$Appended"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView$Prepended"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView$Concat"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView$Take"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView$TakeRight"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView$Drop"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView$DropRight"),
    ProblemFilters.exclude[MissingClassProblem](s"scala.collection.mutable.CheckedIndexedSeqView$$Map"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView$Reverse"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView$Slice"),
  )

  override val buildSettings = Seq(
    mimaFailOnNoPrevious := false, // we opt everything out, knowing we only check library/reflect
  )

  val mimaSettings: Seq[Setting[_]] = Def.settings(
    mimaPreviousArtifacts       := mimaReferenceVersion.value.map(organization.value % name.value % _).toSet,
    mimaCheckDirection          := "both",
    mimaBinaryIssueFilters     ++= mimaFilters,
//  mimaReportSignatureProblems := true, // TODO: enable
  )
}
