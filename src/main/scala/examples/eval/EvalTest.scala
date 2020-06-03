package examples.eval

import cats.Eval

object EvalTest extends App {
  // Now - Eager & Memorized computation happen immediately and after the result are cached
  // Aka val`s
  val now: Eval[Double] = Eval.now(Math.random * 100)

  // Later - Lazy & Memorized computation happen only on access and after the result are cached
  // Aka lazy val`s
  val later: Eval[Double] = Eval.later(Math.random * 100)

  // Always - Lazy computation happen only on access and result computes each time
  // Aka def`s
  val always: Eval[Double] = Eval.always(Math.random * 100)

  println(now)
  println(now.value, now.value, now.value)

  println(later)
  println(later.value, later.value, later.value)

  println(always)
  println(always.value, always.value, always.value)
}
