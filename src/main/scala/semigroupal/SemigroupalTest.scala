package semigroupal

import cats.instances.option._
import cats.syntax.show._
import cats.{Semigroupal, Show}

object SemigroupalTest extends App {
  val op1: Option[(Int, String)] = Semigroupal[Option].product(Some(123), Some("abc"))
  val op2: Option[(Int, String)] = Semigroupal[Option].product(Some(123), None)
  val op3: Option[(Int, String)] = Semigroupal[Option].product(None, Some("abc"))

  implicit def showTuple2[A, B](implicit Sa: Show[A], Sb: Show[B]): Show[(A, B)] =
    (f: (A, B)) => s"(${Sa.show(f._1)}, ${Sb.show(f._2)})"

  println(op1.show)
  println(op2.show)
  println(op3.show)

  val op4: Option[(Int, Int, Int)] = Semigroupal.tuple3(Option(1), Option(2), Some(3))
  val op5: Option[(Int, Int, Int)] = Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int])

  implicit def showTuple3[A, B, C](implicit Sa: Show[A], Sb: Show[B], Sc: Show[C]): Show[(A, B, C)] =
    (f: (A, B, C)) => s"(${Sa.show(f._1)}, ${Sb.show(f._2)}, ${Sc.show(f._3)})"

  println(op4.show)
  println(op5.show)

  val op6: Option[Int] = Semigroupal.map3(Option(1), Option(2), Some(3))(_ + _ + _)
  val op7: Option[Int] = Semigroupal.map3(Option(1), Option(2), Option.empty[Int])(_ + _ + _)

  println(op6.show)
  println(op7.show)
}
