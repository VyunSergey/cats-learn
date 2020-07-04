package examples.check

import cats.syntax.applicative._
import cats.syntax.either._
import cats.syntax.semigroup._
import cats.{Applicative, Semigroup}

final case class CheckF[F[_], E, A](func: A => Either[E, F[A]]) {
  def apply(a: A): Either[E, F[A]] = func(a)

  def and(that: CheckF[F, E, A])(implicit E: Semigroup[E], F: Applicative[F]): CheckF[F, E, A] =
    CheckF { a =>
      (this(a), that(a)) match {
        case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
        case (Left(e), Right(_)) => e.asLeft
        case (Right(_), Left(e)) => e.asLeft
        case (Right(_), Right(_)) => a.pure[F].asRight
      }
  }
}
