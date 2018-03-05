package org.scalameta.scalagen

import scala.meta.Tree
import scala.meta.gen.Generator

sealed trait GenerationFailure {
  def message: String
}

case class GenerationAbortedFailure(message: String) extends GenerationFailure

sealed trait InternalGenerationFailure extends GenerationFailure {
  def message: String = "Internal scalagen error detected: Please report this.\n\n" + errorMessage
  def errorMessage: String
}

case object NoGenerationTasksFailure extends InternalGenerationFailure {
  override val errorMessage: String = "GenerationTask list was empty"
}
