package examples.superadder

import cats.Monoid

trait SuperAdder[A] {
  def add(items: List[A])(implicit m: Monoid[A]): A = {
    items.foldLeft(m.empty)(m.combine)
  }
}

object SuperAdder {
  def apply[A](implicit m: Monoid[A]): SuperAdder[A] = new SuperAdder[A] {}
}
