package semigroup

trait MySemigroup[A] {
  def combine(a: A, b: A): A
}

object MySemigroup {
  def apply[A](implicit s: MySemigroup[A]): MySemigroup[A] = s
}
