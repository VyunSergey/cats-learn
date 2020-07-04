package examples.predicate

import cats.data.Validated

object PredicateTest extends App {
  val positive: Predicate[List[String], Int] = Pure {
    a => Validated.cond(a > 0, a, List(s"Value $a must be positive"))
  }

  val even: Predicate[List[String], Int] = Pure {
    a => Validated.cond(a % 2 == 0, a, List(s"Value $a must be even"))
  }

  val positiveAndEven: Predicate[List[String], Int] = positive.and(even)
  val positiveOrEven: Predicate[List[String], Int] = positive.or(even)

  val values = List(-1, 0, 1, 2, 3, 4, 5)

  values.foreach(i => println(positiveAndEven(i)))

  println()
  values.foreach(i => println(positiveOrEven(i)))
}
