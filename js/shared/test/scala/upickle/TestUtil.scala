package upickle

import utest._

case class TestUtil(pckl: Upickle) {
  def rw[T: Reader: Writer](t: T, s: String*) = {
    rwk[T, T](t, s:_*)(t => t)
  }

  def rwk[T: Reader: Writer, V](t: T, sIn: String*)(k: T => V) = {
    val writtenT = pckl.write(t)

    val strings = sIn.map(_.trim)

    if (strings.length > 0) assert(strings.contains(writtenT))

    for (s <- strings) {
      val readS = pckl.read[T](s)
      assert(k(readS) == k(t))
    }

    assert(k(pckl.read[T](writtenT)) == k(t))
  }
}
