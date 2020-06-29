package applicative

import cats._
import cats.data.{Reader, State, Writer}
import cats.syntax.applicative._

object ApplicativeTest extends App {
  val i: Id[Int] = 123.pure[Id]
  val li: List[Int] = 123.pure[List]
  val oi: Option[Int] = 123.pure[Option]
  val ei: Either[String, Int] = 123.pure[Either[String, *]]
  val ri: Reader[String, Int] = 123.pure[Reader[String, *]]
  val wi: Writer[String, Int] = 123.pure[Writer[String, *]]
  val si: State[String, Int] = 123.pure[State[String, *]]

  println(i)
  println(li)
  println(oi)
  println(ei)
  println(ri)
  println(wi)
  println(si)
}
