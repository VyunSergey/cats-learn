package examples.predicate

import cats.Semigroup
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.apply._
import cats.syntax.semigroup._

sealed trait Predicate[E, A] {
  def apply(a: A)(implicit E: Semigroup[E]): Validated[E, A] = this match {
    case Pure(func) => func(a)
    case And(left, right) => (left(a), right(a)).mapN((_, _) => a)
    case Or(left, right) => left(a) match {
      case Valid(_) => Valid(a)
      case Invalid(e1) => right(a) match {
        case Valid(_) => Valid(a)
        case Invalid(e2) => Invalid(e1 |+| e2)
      }
    }
  }

  def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)
  def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)
}

final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]
