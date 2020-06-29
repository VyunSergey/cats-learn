package apply

import cats._

trait MyApply[F[_]] extends Semigroupal[F] with Functor[F] {
  def ap[A, B](f: F[A => B])(fa: F[A]): F[B]

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    ap(map(fa)(a => (b: B) => (a, b)))(fb)
}
