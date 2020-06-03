package monaderror

import cats.MonadError
import cats.instances.either._
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.monadError._

object MonadErrorTest extends App {
  type ErrorOr[A] = Either[String, A]

  val monadError: MonadError[ErrorOr, String] = MonadError[ErrorOr, String]

  val success: ErrorOr[Int] = monadError.pure(123)
  val success2: ErrorOr[Int] = 123.pure
  println(success)
  println(success2)

  val failure: ErrorOr[Int] = monadError.raiseError("Error")
  val failure2: ErrorOr[Int] = "Error".raiseError[ErrorOr, Int]
  println(failure)
  println(failure2)

  val handled: ErrorOr[Int] = monadError.handleError(failure) {
    case "Error" => 1
    case _ => 0
  }
  val handled2: ErrorOr[Int] = failure.handleError {
    case "Error" => 1
    case _ => 0
  }
  println(handled)
  println(handled2)

  val ensured: ErrorOr[Int] = monadError.ensure(success)("Value is < 1000")(_ < 1000)
  val ensured2: ErrorOr[Int] = success.ensure("Value is < 1000")(_ < 1000)
  println(ensured)
  println(ensured2)

}
