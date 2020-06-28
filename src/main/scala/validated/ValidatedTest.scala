package validated

import cats.Show
import cats.data.{NonEmptyVector, Validated}
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.apply._
import cats.syntax.validated._

object ValidatedTest extends App {
  val v1: Validated.Valid[Int] = Validated.Valid(123)
  val i1: Validated.Invalid[List[String]] = Validated.Invalid(List("Error"))

  println(v1.show[List[String], Int])
  println(i1.show[List[String], Int])

  val v2: Validated[List[String], Int] = Validated.valid[List[String], Int](123)
  val i2: Validated[List[String], Int] = Validated.invalid[List[String], Int](List("Error"))

  println(v2.show)
  println(i2.show)

  val v3: Validated[List[String], Int] = 123.valid[List[String]]
  val i3: Validated[List[String], Int] = List("Error").invalid[Int]

  println(v3.show)
  println(i3.show)

  type ErrorsOr[A] = Validated[List[String], A]

  val v4: ErrorsOr[Int] = 123.pure[ErrorsOr]
  val i4: ErrorsOr[Int] = List("Error").raiseError[ErrorsOr, Int]

  println(v4.show)
  println(i4.show)

  val v5 = Validated.catchOnly[NumberFormatException]("123".toInt)
  val i5 = Validated.catchOnly[NumberFormatException]("abc".toInt)

  implicit val showForNumberFormatException: Show[NumberFormatException] =
    (e: NumberFormatException) => s"NumberFormatException: ${e.getMessage}"

  println(v5.show)
  println(i5.show)

  val v6 = Validated.catchNonFatal[Int]("123".toInt)
  val i6 = Validated.catchNonFatal[Nothing](sys.error("Error"))

  println(v6)
  println(i6)

  val v7 = Validated.fromTry[Int](scala.util.Try("123".toInt))
  val i7 = Validated.fromTry[Int](scala.util.Try("abc".toInt))

  println(v7)
  println(i7)

  val v8 = Validated.fromEither[List[String], Int](Right("123".toInt))
  val i8 = Validated.fromEither[List[String], Int](Left(List("Error")))

  println(v8)
  println(i8)

  val v9 = Validated.fromOption[List[String], Int](scala.util.Try("123".toInt).toOption, List("Error"))
  val i9 = Validated.fromOption[List[String], Int](scala.util.Try("abc".toInt).toOption, List("Error"))

  println(v9)
  println(i9)

  val vTupled: Validated[NonEmptyVector[String], (Int, Int)] = (
    NonEmptyVector.of("Error1").invalid[Int],
    NonEmptyVector.of("Error2").invalid[Int]
  ).tupled

  println(vTupled.show)

  val vMap: Validated[String, Int] = 123.valid[String].map(_ * 100)
  val iMap: Validated[String, Int] = "Error".invalid[Int].leftMap(_ + "!")

  println(vMap.show)
  println(iMap.show)

  val vBimap: Validated[String, Int] = 123.valid[String].bimap(_ + "!", _ * 100)
  val iBimap: Validated[String, Int] = "Error".invalid[Int].bimap(_ + "!", _ * 100)

  println(vBimap.show)
  println(iBimap.show)

/**
 * We can`t do for-comprehensions with `flatMap` because Validated isn`t a Monad
 * Example:
  val vRes: Validated[String, Int] = for {
    a <- 123.valid[String]
    b <- 456.valid[String]
  } yield a + b
 * Instead of that Cats provides `andThen` method as stand-in for `flatMap`
 */
  val vRes: Validated[String, Int] = 123.valid[String].andThen { a =>
    456.valid[String].map { b =>
      a + b
    }
  }
  println(vRes.show)
}
