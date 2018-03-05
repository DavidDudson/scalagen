package org.scalameta.scalagen

import scala.meta._
import scala.meta.gen._

object Runner {

  def apply(globalGenerationContext: GlobalGenerationContext, possibleGenerators: Set[Generator]): Option[Tree] = {
    val initialTaskMap = TreeAnalyzer(globalGenerationContext, possibleGenerators).getTaskMap

    initialTaskMap
  }

}