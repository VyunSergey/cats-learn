package examples

import cats.instances.int._

object SuperAdderTest extends App {
  val lst: List[Int] = List(1, 2, 3, 4)
  val opLst: List[Option[Int]] = lst.map(Option(_))
  val opLstWithNone: List[Option[Int]] = opLst :+ None
  val strLst: List[String] = opLstWithNone.map(_.fold("")(_.toString))

  println(s"add [${lst.mkString(", ")}]: " + SuperAdder[Int].add(lst))
  println(s"add [${opLst.mkString(", ")}]: " + SuperAdder[Option[Int]].add(opLst))
  println(s"add [${opLstWithNone.mkString(", ")}]: " + SuperAdder[Option[Int]].add(opLstWithNone))
  println(s"add [${strLst.mkString(", ")}]: " + SuperAdder[String].add(strLst))

}
