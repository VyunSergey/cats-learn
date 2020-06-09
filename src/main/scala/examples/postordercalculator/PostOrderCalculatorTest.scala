package examples.postordercalculator

import cats.data.State

object PostOrderCalculatorTest extends App {
  import PostOrderCalculator._

  val (s1, r1) = evalOne("42").run(Nil).value
  println(s"State: $s1 Result: $r1")
  println()

  // (1 + 2) * 3 = 9
  val testProgram: State[List[Int], Int] = for {
    _   <- evalOne("1")
    _   <- evalOne("2")
    _   <- evalOne("+")
    _   <- evalOne("3")
    ans <- evalOne("*")
  } yield ans
  val (s2, r2) = testProgram.run(Nil).value
  println(s"State: $s2 Result: $r2")
  println()

  // (1 + 2) * 3 = 9
  val simpleProgram: State[List[Int], Int] =
    evalAll("1 2 + 3 *".split(" ").toList, verbose = true)
  val (s3, r3) = simpleProgram.run(Nil).value
  println(s"State: $s3 Result: $r3")
  println()

  // ((1 + 2) - 1) * 2 - (5 - 3) * 3 = -2
  val hardProgram: State[List[Int], Int] =
    evalAll("1 2 + 1 - 2 * 5 3 - 3 * -".split(" ").toList, verbose = true)
  val (s4, r4) = hardProgram.run(Nil).value
  println(s"State: $s4 Result: $r4")
  println()

}
