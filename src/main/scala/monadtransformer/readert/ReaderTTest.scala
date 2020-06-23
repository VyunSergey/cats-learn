package monadtransformer.readert

import cats.data.Kleisli
import cats.instances.list._

import scala.annotation.tailrec

object ReaderTTest extends App {
  type ReaderT[F[_], A, B] = Kleisli[F, A, B]

  val step1: ReaderT[List, Int, Int] = Kleisli(x => List(x + 1, x - 1))
  val step2: ReaderT[List, Int, Int] = Kleisli(x => List(x, -x))
  val step3: ReaderT[List, Int, Int] = Kleisli(x => List(x * 2, x / 2))

  val pipeline: Kleisli[List, Int, Int] = step1 andThen step2 andThen step3
  println(pipeline.run(20))

  def kleisliFibonacci(n: Int): BigInt = {
    type FibonacciProducer[A] = Kleisli[List, (A, A), (A, A)]
    val producer: FibonacciProducer[BigInt] = Kleisli({case (a, b) => List((b, a + b))})

    @tailrec
    def composeN(n: Int, func: FibonacciProducer[BigInt] = producer): FibonacciProducer[BigInt] =
      if (n > 0) composeN(n - 1, func andThen producer) else func

    if (n > 2) composeN(n - 2).run((1, 1)).head._1
    else if (n > 0) 1
    else 0
  }

  println((0 to 20).map(kleisliFibonacci))
}
