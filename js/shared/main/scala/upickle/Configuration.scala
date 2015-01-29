package upickle

import acyclic.file
import scala.reflect.ClassTag

/** Configuration for macro-generated picklers. */
trait Configuration {
  /* Configure the sum encoding by implementing these two methods. */
  def annotate[T: ClassTag](rw: Reader[T], n: String): Reader[T]
  def annotate[T: ClassTag](rw: Writer[T], n: String): Writer[T]
}

object Configuration {
  case object Default extends Configuration {
    def annotate[T: ClassTag](rd: Reader[T], n: String) = Reader[T] {
      case Js.Arr(Js.Str(`n`), x) => rd.read(x)
    }

    def annotate[T: ClassTag](wr: Writer[T], n: String) = Writer[T] {
      case x: T => Js.Arr(Js.Str(n), wr.write(x))
    }
  }
}
