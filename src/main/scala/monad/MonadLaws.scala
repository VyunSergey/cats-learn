package monad

import cats.Monad

object MonadLaws {
  def leftIdentityLaw[F[_], A, B](a: A)(f: A => F[B])(implicit M: Monad[F]): Boolean = {
    M.flatMap(M.pure(a))(f) == f(a)
  }

  def rightIdentityLaw[F[_], A, B](a: A)(f: A => F[B])(implicit M: Monad[F]): Boolean = {
    M.flatMap(f(a))(M.pure) == f(a)
  }

  def associativeLaw[F[_], A, B, C](a: A)(f: A => F[B])(g: B => F[C])(implicit M: Monad[F]): Boolean = {
    M.flatMap(M.flatMap(M.pure(a))(f))(g) == M.flatMap(M.pure(a))((x: A) => M.flatMap(f(x))(g))
  }
}
