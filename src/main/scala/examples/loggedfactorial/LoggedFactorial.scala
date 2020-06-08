package examples.loggedfactorial

import cats.Show
import cats.data.Writer
import cats.syntax.writer._

import scala.concurrent._
import scala.concurrent.duration._

object LoggedFactorial extends App {
  def slowly[A](body: => A): A = {
    try body finally Thread.sleep(100)
  }

  def factorial(n: Int): BigInt = {
    val res = slowly(if (n == 0) BigInt(1) else n * factorial(n - 1))
    println(s"fact $n: $res")
    res
  }

  val fact5 = factorial(5)
  println(fact5)

  import scala.concurrent.ExecutionContext.Implicits.global
  val fact5Future1 = Future(factorial(5))
  val fact5Future2 = Future(factorial(5))
  val res = Await.result(Future.sequence(Vector(fact5Future1, fact5Future2)), 10.seconds)
  println(res)

  type Logged[A] = Writer[Vector[String], A]

  def loggedFactorial(n: Int): Logged[BigInt] = {
    if (n == 0) BigInt(1).writer(Vector(s"fact $n: 1"))
    else loggedFactorial(n - 1).mapBoth({(log, res) =>
      val newRes: BigInt = n * res
      (log :+ s"fact $n: $newRes", newRes)
    })
  }

  val lFact5 = loggedFactorial(5)

  implicit val showLogged: Show[(Vector[String], BigInt)] = Show.show({(tp: (Vector[String], BigInt)) =>
    val (log, res) = tp
    (log :+ res).mkString("\n")
  })
  println(lFact5.show)

  val lFact5Future1 = Future(loggedFactorial(5))
  val lFact5Future2 = Future(loggedFactorial(5))
  val lRes = Await.result(Future.sequence(Vector(lFact5Future1, lFact5Future2)), 10.seconds)
  println(lRes.map(_.mapWritten(_.map("LOG: \"" + _ + "\""))).map(_.show).mkString("Computations:\n[", "]\n[", "]"))

}
