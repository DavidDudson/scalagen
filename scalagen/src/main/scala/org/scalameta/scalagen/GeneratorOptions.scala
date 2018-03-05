package org.scalameta.scalagen

import scala.meta.Tree

//TODO: Add semantic database etc.
case class GlobalGenerationContext(root: Tree, depthInRoot: Int, options: GeneratorOptions)
case class GenerationContext(raw: Tree, globalGenerationContext: GlobalGenerationContext)

sealed trait ScaladocOption

case object All extends ScaladocOption
case object NoDocs extends ScaladocOption
case object Children extends ScaladocOption
case object Self extends ScaladocOption

case class GeneratorOptions(continueOnError: Boolean = false, retainAllScaladoc: Boolean)

case class LocalGeneratorOptions(scaladocOption: ScaladocOption = NoDocs)
