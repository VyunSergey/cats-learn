package equal

import java.util.Date

import cats.Eq
import cats.instances.int._
import cats.instances.option._
import cats.syntax.eq._
import cats.syntax.show._
import cats.syntax.option._
import show.ShowInstances._
import EqInstances._
import printable.Cat

object EqTest extends App {
  val eqInt = Eq[Int]
  println("123 == 123: " + eqInt.eqv(123, 123))
  println("123 == 123: " + (123 === 123))
  println("123 != 456: " + (123 =!= 456))

  val listOp: List[Option[Int]] = List(1, 2, 3).map(Option(_))
  val listFl: List[Option[Int]] = listOp.filter(i => i === 1.some)

  println(listOp)
  println(listFl)

  val d1 = new Date()
  Thread.sleep(1)
  val d2 = new Date()

  println(d1, d1.getTime)
  println(d2, d2.getTime)
  println(d1 === d1)
  println(d1 =!= d2)

  val cat1: Cat = Cat("Margo", 11, "black")
  val cat2: Cat = Cat("Garfield", 38, "orange and black")

  println("cat1: " + cat1.show)
  println("cat2: " + cat2.show)
  println("cat1 === cat2: " + (cat1 === cat2))
  println("cat1 =!= cat2: " + (cat1 =!= cat2))

  val someCat: Option[Cat] = Some(cat1)
  val emptyCat: Option[Cat] = Option.empty[Cat]

  println("someCat === someCat: " + (someCat === someCat))
  println("someCat === emptyCat: " + (someCat === emptyCat))
  println("someCat =!= emptyCat: " + (someCat =!= emptyCat))
}
