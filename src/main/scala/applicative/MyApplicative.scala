package applicative

import cats._

trait MyApplicative[F[_]] extends Semigroupal[F] with Functor[F] {
  def pure[A](a: A): F[A]
}
