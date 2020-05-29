package monoid

import cats.Monoid
import cats.syntax.monoid._
import MonoidLaws._

object MonoidTest extends App {
  val i = 1
  val j = 2
  val k = 3
  val mInt: Monoid[Int] = Monoid[Int]

  println(s"i: $i")
  println(s"j: $j")
  println(s"k: $k")
  println("i + j: " + mInt.combine(i, j))
  println("i |+| j: " + (i |+| j))
  println("(i + j) + k: " + mInt.combine(mInt.combine(i, j), k))
  println("i + (j + k): " + mInt.combine(i, mInt.combine(j, k)))
  println("i |+| j |+| k: " + (i |+| j |+| k))

  println("check Associative Law for Int: " + associativeLaw(i, j, k))

  println(s"empty: ${mInt.empty}")
  println(s"Is empty: ${mInt.empty.isEmpty}")
  println("i + empty: " + mInt.combine(i, mInt.empty))
  println("i |+| empty: " + (i |+| mInt.empty))

  println("check Identity Law for Int: " + identityLaw(i))

  val s1 = "Hello "
  val s2 = "World "
  val s3 = "with Cats!"
  val mString: Monoid[String] = Monoid[String]

  println(mString.combine(mString.combine(s1, s2), s3))
  println(s1 |+| s2 |+| s3)
  println(List(s1, s2, s3).foldLeft(mString.empty)(mString.combine))
  println(List(s1, s2, s3).reduce(mString.combine))

  println("check Associative Law for String: " + associativeLaw(s1, s2, s3))
  println("check Identity Law for String: " + identityLaw(s1))

}
