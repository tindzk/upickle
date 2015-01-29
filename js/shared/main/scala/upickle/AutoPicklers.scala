package upickle

import scala.language.experimental.macros

import acyclic.file

/**
 * As the implementation of Scala macros is still experimental, auto-generated
 * picklers must be explicitly mixed in.
 */
trait AutoPicklers extends GeneratedInternal { this: Upickle =>
  val config: Configuration

  implicit val self: AutoPicklers = this

  implicit def macroR[T]: Reader[T] = macro Macros.macroRImpl[T]
  implicit def macroW[T]: Writer[T] = macro Macros.macroWImpl[T]
}
