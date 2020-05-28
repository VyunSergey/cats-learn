package monoids

import semigroup.MySemigroup

trait MyMonoid[A] extends MySemigroup[A] {
  def empty: A
}

object MyMonoid {
  def apply[A](implicit m: MyMonoid[A]): MyMonoid[A] = m
}
