package functor

import cats.Functor

object FunctorLaws {
  def identityLaw[F[_], A](fa: F[A])(implicit F: Functor[F]): Boolean = {
    F.map(fa)(identity) == fa
  }

  def compositionLaw[F[_], A, B, C](fa: F[A])(f: A => B)(g: B => C)(implicit F: Functor[F]): Boolean = {
    F.map(fa)(g compose f) == F.map(F.map(fa)(f))(g)
  }
}
