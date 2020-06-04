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

  // recursion method for computing Factorial: n! = n * (n - 1) * ... * 1
  def factorial(n: BigInt): BigInt = {
    if (n == 1) n else n * factorial(n - 1)
  }

  @scala.annotation.tailrec
  def tailRecFactorial(n: BigInt, result: BigInt = 1): BigInt = {
    if (n == 1) result else tailRecFactorial(n - 1, n * result)
  }

  // Eval provides methods map and flatMap which are trampolined
  // That means we can nested calls to map and flatMap and it will Stack Safe
  // Defer provides a delayed computation which is Stack Safe
  def stackSafeFactorial(n: BigInt): Eval[BigInt] = {
    if (n == 1) Eval.now(n)
    else Eval.defer(stackSafeFactorial(n - 1)).map(_ * n)
  }
  println(factorial(5))
  println(tailRecFactorial(5))
  println(stackSafeFactorial(5).value)

  try {
    println(factorial(10000))
  } catch {
    case _: java.lang.StackOverflowError => println("java.lang.StackOverflowError")
    case _: Throwable => println("Other Error")
  }
  println(tailRecFactorial(10000))
  println(stackSafeFactorial(10000).value)

  println(now)
  println(now.value, now.value, now.value)

  println(later)
  println(later.value, later.value, later.value)

  println(always)
  println(always.value, always.value, always.value)

  val greeting = Eval.always {
    println("Step 1")
    "Hello"
  }.map { str =>
    println("Step 2")
    s"$str World!"
  }
  println()
  Thread.sleep(1000)
  println(greeting.value)

  val ans = for {
    a <- Eval.always {println("Calculating A"); 40}
    b <- Eval.now { println("Calculating B"); 2}
  } yield {
    println("Adding A + B")
    a + b
  }
  println()
  Thread.sleep(1000)
  println(ans.value)

  val saying = Eval.always {
    println("Step 1"); "The cat"
  }.map { str =>
    println("Step2"); s"$str sat on"
  }.memoize
    .map { str =>
    println("Step3"); s"$str the mat"
  }
  println()
  Thread.sleep(1000)
  println(saying.value)
  println(saying.value)
}
