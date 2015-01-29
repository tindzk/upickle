package upickle

import acyclic.file

/**
 * Basic functionality to be able to read and write objects. Kept as a trait so
 * other internal files can use it. A pickler for T consists of an instance
 * Reader[T] and Writer[T]. uPickle tries the following mechanisms for pickling
 * a type:
 *
 * - Finding a default pickler, see [[Picklers]]
 * - Generating a pickler using macros for companion types that have an apply()
 *   and unapply() method, see [[AutoPicklers]]
 */
trait Upickle {
  /**
   * Serialize an object of type [[T]] to a [[String]]
   */
  @inline def write[T: Writer](expr: T): String = json.write(writeJs(expr))

  /**
   * Serialize an object of type [[T]] to a [[Js.Value]]
   */
  @inline def writeJs[T: Writer](expr: T): Js.Value = implicitly[Writer[T]].write(expr)

  /**
   * Deserialize a [[String]] object of type [[T]]
   */
  @inline def read[T: Reader](expr: String): T = readJs[T](json.read(expr))

  /**
   * Deserialize a [[Js.Value]] object of type [[T]]
   */
  @inline def readJs[T: Reader](expr: Js.Value): T = implicitly[Reader[T]].read(expr)
}
