package examples.postordercalculator

import cats.data.State
import cats.syntax.applicative._

import scala.util.matching.Regex

object PostOrderCalculator {
  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String, verbose: Boolean = false): CalcState[Int] = {
    val digitsPattern: Regex = "([0-9]*)".r
    sym match {
      case "+" => operator(_ + _, verbose)
      case "-" => operator(_ - _, verbose)
      case "*" => operator(_ * _, verbose)
      case "/" => operator(_ / _, verbose)
      case digitsPattern(num) => operand(num.toInt, verbose)
      case other => sys.error(s"Can not parse symbol: $other")
    }
  }

  def operator(f: (Int, Int) => Int, verbose: Boolean = false): CalcState[Int] =
    State[List[Int], Int]({
      case a :: b :: tail =>
        val ans = f(a, b)
        if (verbose) println(s"Stack: [${tail.mkString(", ")}], Result: $ans")
        (ans :: tail, ans)
      case stack =>
        if (verbose) println(s"Stack: [${stack.mkString(", ")}], Result: ???")
        sys.error(s"Not in Post Order position: [${stack.mkString(", ")}]")
    })

  def operand(num: Int, verbose: Boolean = false): CalcState[Int] =
    State[List[Int], Int]({ stack =>
      val newStack = num :: stack
      if (verbose) println(s"Stack: [${newStack.mkString(", ")}], Result: $num")
      (newStack, num)
    })

  def evalAll(input: List[String], verbose: Boolean = false): CalcState[Int] =
    input.foldLeft[CalcState[Int]](0.pure[CalcState])({
      case (state, sym) => state.flatMap(_ => evalOne(sym, verbose))
    })

}
