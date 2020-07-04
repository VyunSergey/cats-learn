package examples.parallelfoldmap

import examples.parallelfoldmap.ParallelFoldMapInstances._
import examples.timed.Timed
import examples.timed.TimedInstances._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object ParallelFoldMapTest extends App {
  val firstElement: BigInt = BigInt(0)
  val numElements: BigInt = BigInt(10000000)

  val vecValues: Vector[BigInt] = Vector.range(firstElement, numElements)
  println(s"Vector sample values: ${vecValues.take(10).mkString(", ")}, ...")

  val vecFolded: Future[BigInt] = ParallelFoldMap[Vector].foldMap(vecValues)(identity)
  val vecRes: BigInt = Timed[BigInt].time(Await.result(vecFolded, 10.second))
  println(s"Vector Actual: $vecRes Expected: ${vecValues.sum}")

  println()

  val streamValues: Stream[BigInt] = Vector.range(firstElement, numElements).toStream
  println(s"Stream sample values: ${streamValues.take(10).mkString(", ")}, ...")

  val streamFolded: Future[BigInt] = ParallelFoldMap[Stream].foldMap(streamValues)(identity)
  val streamRes: BigInt = Timed[BigInt].time(Await.result(streamFolded, 10.second))
  println(s"Set Actual: $streamRes Expected: ${streamValues.sum}")
}
