package org.scalameta.scalagen

import scala.collection.immutable.SortedMap
import scala.meta.{Mod, Tree, Type}
import scala.meta.gen.Generator
import scala.meta.gen.TypeclassExtractors.retrieveAnnotExtractInstance

case class TreeAnalyzer(globalGenerationContext: GlobalGenerationContext, possibleGenerators: Set[Generator]) {

  type TaskMap = SortedMap[Int, Map[Tree, List[GenerationTask]]]

  val taskMapGenerator: (Tree, Int) => TaskMap =
    (t, i) => {
      val tasks =
        gatherGenerators(t)
          .map(SimpleGenerationTask(GenerationContext(t, globalGenerationContext), _))

      if (tasks.isEmpty) {
        SortedMap()
      } else {
        SortedMap(i -> Map(t -> tasks))
      }
    }

  def getTaskMap: TaskMap = LevelBasedTreeReducer.reduce(globalGenerationContext.root, taskMapGenerator, _ ++ _)

  private def getMatchingGenerator(a: Mod.Annot): Option[Generator] =
    a.init.tpe match {
      case Type.Name(value) =>
        possibleGenerators.find(_.name == value)
      case _ => None
    }

  def gatherGenerators(tree: Tree): List[Generator] =
    retrieveAnnotExtractInstance(tree)
      .map(_.extract(tree).flatMap(getMatchingGenerator(_).toList))
      .getOrElse(Nil)

  private def hasGenerator(tree: Tree): Boolean =
    gatherGenerators(tree).nonEmpty
}
