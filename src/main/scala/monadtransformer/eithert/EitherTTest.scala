package monadtransformer.eithert

import cats.Show
import cats.data.EitherT
import cats.instances.list._
import cats.syntax.applicative._

object EitherTTest extends App {
  type ListEither[A] = EitherT[List, String, A]

  val res1: ListEither[Int] = EitherT[List, String, Int](List(Right(42)))
  val res2: ListEither[Int] = 42.pure[ListEither]
  val res3: ListEither[Int] = EitherT.fromEither(Right(42))
  val res4: ListEither[Int] = EitherT.liftF(List(1, 2, 3))
  val res5: ListEither[Int] = EitherT(res4.value ::: List(Left(""), Left("")))

  implicit val listEitherShow: Show[List[Either[String, Int]]] = (lsi: List[Either[String, Int]]) =>
    s"result: ${lsi.map(_.map(_.toString).getOrElse("NULL")).foldLeft("")(_ + "," + _).tail}"

  println(res1.show)
  println(res2.show)
  println(res3.show)
  println(res4.show)
  println(res5.show)

  val res6 = for {
    a <- res1
    b <- res2
    c <- res3
    d <- res4
    e <- res5
  } yield a - b + c + d - e

  println(res6.show)

}
