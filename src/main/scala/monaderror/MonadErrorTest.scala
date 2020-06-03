package monaderror

import cats.MonadError
import cats.instances.either._

object MonadErrorTest extends App {
  type ErrorOr[A] = Either[String, A]

  val monadError: MonadError[ErrorOr, String] = MonadError[ErrorOr, String]

  val success: ErrorOr[Int] = monadError.pure(123)
  println(success)
  val failure: ErrorOr[Int] = monadError.raiseError("Error")
  println(failure)

  val handled: ErrorOr[Int] = monadError.handleError(failure) {
    case "Error" => 1
    case _ => 0
  }
  println(handled)

  val ensured: ErrorOr[Int] = monadError.ensure(success)("Value is < 1000")(_ < 1000)
  println(ensured)

}
