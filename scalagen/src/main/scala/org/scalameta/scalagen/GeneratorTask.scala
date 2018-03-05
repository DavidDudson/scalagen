package org.scalameta.scalagen

import scala.meta.{Stat, Tree}
import scala.meta.gen._

sealed trait GenerationTask {
  def run: Either[GenerationFailure, GenerationResult]
}

case class SimpleGenerationTask(ctx: GenerationContext, generatorToApply: Generator) extends GenerationTask {

  override def run(current: Tree): Either[GenerationFailure, GenerationResult] = {
    val companions: List[Stat] = CompanionGeneratorApi.partialExtender(generatorToApply).applyOrElse(current, _ => Nil)
    val extended: List[Stat] = ExtensionGeneratorApi.partialExtender(generatorToApply).applyOrElse(current, _ => Nil)
    val manipulated: Option[Tree] = ManipulationGeneratorApi.partialManipulator(generatorToApply).lift(current)
    val transmuted: List[Stat] = TransmutationGeneratorApi.partialTransmutator(generatorToApply).applyOrElse(current, _ => Nil)

    val wasExtended = extended.nonEmpty
    val wasManipulated = manipulated.exists(_ ne current)
    val wasTransmuted =
      transmuted match {
        case h :: Nil if h eq current => false
        case _ => true
      }

    if (wasTransformedMultipleTimes(wasExtended, wasManipulated, wasTransmuted)) {
      abortDueToMultipleTransform(current, generatorToApply)
    } else if (wasExtended) {
      val res =
        for {
          rev <- TypeclassExtractors.retrieveStatReplaceInstance(current)
          eev <- TypeclassExtractors.retrieveStatExtractInstance(current)
        } yield current.prepend(extended)(rev, eev)

      res match {
        case None => abort("Failed to extract necessary instances")
        case Some(out) => GenerationResult(this, out, companions)
      }
    } else if (wasManipulated) {
      GeneratorOutputContext(current, manipulated.get, companions)
    } else if (wasTransmuted) {
      GeneratorOutputContext(current, current, companions, Map((getParentOrAbort(current), transmuted)))
    } else {
      GeneratorOutputContext(current, current, companions)
    }
  }

  private def getParentOrAbort(t: Tree): Tree =
    t.parent.getOrElse(abort(s"Expected the following tree to have a parent \n\n ${t.syntax}"))

  private def abortDueToMultipleTransform(t: Tree, g: Generator) = {
    abort(
      s"""Generator: ${g.name} tried to transform a tree multiple times
         |
       |This error can appear when a generator implements more then one of the following
         |
       | extend()
         | transmute()
         | manipulate()
         |
       | ========================
         |        Tree
         | ========================
         | ${t.syntax}
         | ========================
         |
     """.stripMargin)
  }

  private def wasTransformedMultipleTimes(booleans: Boolean*) =
    booleans.count(_ == true) > 1
}


case class Transumutation(list: SimpleGenerationTask) extends GenerationTask

case class GenerationResult(task: GenerationTask,
                            output: Tree,
                            logging: List[GenerationLog],
                            extraTasks: List[GenerationTask])

