package semigroupal

import cats.instances.option._
import cats.syntax.apply._
import cats.syntax.semigroup._
import cats.syntax.show._
import cats.{Monoid, Show}
import examples.printable.Cat

object ApplySyntaxTest extends App {
  val op1: Option[(Int, String)] = (Option(123), Option("abc")).tupled

  implicit def showTuple2[A, B](implicit Sa: Show[A], Sb: Show[B]): Show[(A, B)] =
    (f: (A, B)) => s"(${Sa.show(f._1)}, ${Sb.show(f._2)})"
  println(op1.show)

  val op2 = (Option(123), Option("abc"), Some(true)).tupled

  implicit def showTuple3[A, B, C](implicit Sa: Show[A], Sb: Show[B], Sc: Show[C]): Show[(A, B, C)] =
    (f: (A, B, C)) => s"(${Sa.show(f._1)}, ${Sb.show(f._2)}, ${Sc.show(f._3)})"
  println(op2.show)

  val opCat: Option[Cat] = (Option("Margo"), Option(11), Option("black")).mapN(Cat.apply)

  implicit val showCat: Show[Cat] =
    (c: Cat) => s"${c.name} is a ${c.age} year-old ${c.color} cat"
  println(opCat.show)

  // Easy way for creating Monoid instances with Tuples
  val tupleToCat: (String, Int, String) => Cat = Cat.apply
  val catToTuple: Cat => (String, Int, String) = cat => (cat.name, cat.age, cat.color)
  implicit val catMonoid: Monoid[Cat] = (Monoid[String], Monoid[Int], Monoid[String]).imapN(tupleToCat)(catToTuple)

  val cat1: Cat = Cat("Garfield", 12, "lasagne")
  val cat2: Cat = Cat("Margo", 11, "black")
  val sumCat = cat1 |+| cat2

  println(sumCat)
  println(sumCat.show)
}
