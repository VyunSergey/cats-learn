package examples.timed

import examples.timed.TimedInstances._

object TimedTest extends App {
  val vec: Vector[BigInt] = Vector.range(BigInt(0), BigInt(100000000))
  val timed = Timed[BigInt]

  val vecSum = timed.time(vec.sum)
  println(s"Sum: $vecSum")
}
