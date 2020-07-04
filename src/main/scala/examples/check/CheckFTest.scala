package examples.check

import cats.Id
import cats.syntax.either._

object CheckFTest extends App {
  type CheckError[A] = CheckF[Id, List[String], A]

  val check1: CheckError[Int] = CheckF {
    i =>
      if (i > 0) i.asRight
      else List(s"Value $i must be positive").asLeft
  }

  val check2: CheckError[Int] = CheckF {
    i =>
      if (i % 2 == 0) i.asRight
      else List(s"Value $i must be even").asLeft
  }

  val checks = check1 and check2

  println(checks(-1))
  println(checks(0))
  println(checks(1))
  println(checks(2))
}
