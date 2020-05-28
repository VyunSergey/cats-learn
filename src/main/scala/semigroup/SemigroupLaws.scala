package semigroup

import cats.Semigroup

object SemigroupLaws {
  def associativeLaw[A](a: A, b: A, c: A)(implicit inst: Semigroup[A]): Boolean = {
    inst.combine(a, inst.combine(b, c)) == inst.combine(inst.combine(a, b), c)
  }
}
