package upickle
package shared
import utest._
import upickle.{Writer, Reader, Bundle}

class TestUtil(bundle: Bundle){
  import bundle._
  def rw[T: Reader: Writer](t: T, s: String*) = {
    rwk[T, T](t, s:_*)(t => t)
  }
  def rwk[T: Reader: Writer, V](t: T, sIn: String*)(k: T => V) = {
    val writtenT = bundle.write(t)

    val strings = sIn.map(_.trim)

    if (strings.length > 0) assert(strings.contains(writtenT))

    for (s <- strings) {
      val readS = bundle.read[T](s)
      k(readS) == k(t)
    }

    assert(k(bundle.read[T](writtenT)) == k(t))
  }
}
