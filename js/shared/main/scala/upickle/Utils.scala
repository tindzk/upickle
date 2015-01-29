package upickle

import scala.reflect.ClassTag

import acyclic.file

/**
 * Helpers which are exclusively used by the code generator.
 */
private[upickle] trait GenerationUtils {
  protected[this] def readerCaseFunction[T](names: Array[String],
                                            defaults: Array[Js.Value],
                                            read: PartialFunction[Js.Value, T]): PartialFunction[Js.Value, T] =
    Utils.validate("Object"){case x: Js.Obj => read(mapToArray(x, names, defaults))}

  private[this] def arrayToMap(a: Js.Arr, names: Array[String], defaults: Array[Js.Value]) = {
    val accumulated = new Array[(String, Js.Value)](names.length)
    var i = 0
    val l = a.value.length

    while (i < l) {
      if (defaults(i) != a.value(i)) {
        accumulated(i) = names(i) -> a.value(i)
      }
      i += 1
    }

    Js.Obj(accumulated.filter(_ != null): _*)
  }

  private[this] def mapToArray(o: Js.Obj, names: Array[String], defaults: Array[Js.Value]) = {
    val accumulated = new Array[Js.Value](names.length)
    val map = o.value.toMap
    var i = 0
    val l = names.length

    while (i < l) {
      if (map.contains(names(i))) accumulated(i) = map(names(i))
      else if (defaults(i) != null) accumulated(i) = defaults(i)
      else throw new Invalid.Data(o, "Key missing: " + names(i))
      i += 1
    }

    Js.Arr(accumulated: _*)
  }

  protected[this] def RCase[T](names: Array[String],
                               defaults: Array[Js.Value],
                               read: PartialFunction[Js.Value, T]) =
    Reader[T](readerCaseFunction(names, defaults, read))

  protected[this] def WCase[T](names: Array[String],
                               defaults: Array[Js.Value],
                               write: T => Js.Value) =
    Writer[T](x => arrayToMap(write(x).asInstanceOf[Js.Arr], names, defaults))
}

/**
 * APIs that need to be exposed to the outside world to support macros
 * which depend on them, but probably should not get used directly.
 */
object MacrosUtils {
  import Aliases._

  def merge0[T: ClassTag, R, U](f: T => R): U => R = {
    case t: T => f(t)
  }

  def merge[T: ClassTag, R, V: ClassTag, U](f: T => R, g: V => R): U => R = {
    case v: V => g(v)
    case t: T => f(t)
  }

  def knotRW[T, V](f: Knot.RW[T] => V): V = f(new Knot.RW(null, null))
  def knotR[T, V](f: Knot.R[T] => V): V = f(new Knot.R(null))
  def knotW[T, V](f: Knot.W[T] => V): V = f(new Knot.W(null))

  def Case0R[T](t: T) = R[T]({case x => t})
  def Case0W[T](t: T) = W[T](x => Js.Obj())

  def validateReader[T](name: String)(r: Reader[T]): Reader[T] = Reader{
    r.read.orElse { case x => throw Invalid.Data(x, name) }
  }
}

/**
 * General helpers
 */
object Utils {
  def validate[T](name: String)(pf: PartialFunction[Js.Value, T]): PartialFunction[Js.Value, T] = {
    pf.orElse { case x => throw Invalid.Data(x, name) }
  }
}
