package org.scalameta.scalagen

import scala.meta.Message

sealed trait GenerationLog {
  def message: String
}

case class Info(message: Message) extends GenerationLog
case class Warning(message: Message) extends GenerationLog
case class Error(message: Message) extends GenerationLog
case class Abort(message: Message) extends GenerationLog
