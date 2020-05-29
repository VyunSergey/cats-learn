package monoid

import cats.Monoid

object MonoidLaws {
  def associativeLaw[A](a: A, b: A, c: A)(implicit m: Monoid[A]): Boolean = {
    m.combine(a, m.combine(b, c)) == m.combine(m.combine(a, b), c)
  }

  def identityLaw[A](a: A)(implicit m: Monoid[A]): Boolean = {
    (m.combine(a, m.empty) == a) && (m.combine(m.empty, a) == a)
  }

}
