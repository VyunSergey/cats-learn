package semigroup

import cats.Semigroup
import cats.syntax.semigroup._
import SemigroupLaws._

object SemigroupTest extends App {
  val i = 1
  val j = 2
  val k = 3

  val sgInt: Semigroup[Int] = Semigroup[Int]

  println(s"i: $i")
  println(s"j: $j")
  println(s"k: $k")
  println("i + j: " + sgInt.combine(i, j))
  println("i |+| j: " + (i |+| j))
  println("(i + j) + k: " + sgInt.combine(sgInt.combine(i, j), k))
  println("i + (j + k): " + sgInt.combine(i, sgInt.combine(j, k)))
  println("i |+| j |+| k: " + (i |+| j |+| k))

  println("check Associative Law for Int: " + associativeLaw(i, j, k))

}
