package show

import cats.Show
import cats.instances.all._
import cats.syntax.show._
import ShowInstances._
import java.util.Date

import examples.printable.Cat

object ShowTest extends App {
  val i = 123
  val showInt = Show.apply[Int]
  println(showInt.show(i))
  println(i.show)

  val d = new Date()
  val showDate = Show.apply[Date]
  println(showDate.show(d))

  val cat = Cat("Margo", 11, "black")
  val showCat = Show[Cat]
  println(showCat.show(cat))
  println(cat.show)
}
