package traverse

import cats.Applicative
import cats.data.Validated
import cats.instances.future._
import cats.instances.list._
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.traverse._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

object TraverseTest extends App {
  val hostNames: List[String] = List(
    "github.com",
    "yandex.ru",
    "stackoverflow.com"
  )

  def getUpTime(hostName: String): Future[Int] = {
    (hostName.length * 60).pure[Future]
  }

  /**
   * Not using Traverse with for-comprehension
   */
  val allUpTimes: Future[List[Int]] =
    hostNames.foldLeft(List.empty[Int].pure[Future]) {
      (fAcc: Future[List[Int]], host: String) =>
        for {
          acc <- fAcc
          upTime <- getUpTime(host)
        } yield acc :+ upTime
    }

  val res: List[Int] = Await.result(allUpTimes, 1.second)
  println(s"Result with out Traverse: ${res.mkString("[", ", ", "]")}")

  /**
   * Not using Traverse with combiner function
   */
  def hostsCombiner(acc: Future[List[Int]], hostName: String): Future[List[Int]] =
    for {
      acc <- acc
      upTime <- getUpTime(hostName)
    } yield acc :+ upTime

  val allUpTimes2: Future[List[Int]] =
    hostNames.foldLeft(List.empty[Int].pure[Future])(hostsCombiner)

  val res2: List[Int] = Await.result(allUpTimes2, 1.second)
  println(s"Result with out Traverse with Combiner: ${res2.mkString("[", ", ", "]")}")

  /**
   * Not using Traverse with mapN
   */
  def hostsCombiner2(acc: Future[List[Int]], hostName: String): Future[List[Int]] =
    (acc, getUpTime(hostName)).mapN(_ :+ _)

  val allUpTimes3: Future[List[Int]] =
    hostNames.foldLeft(List.empty[Int].pure[Future])(hostsCombiner2)

  val res3: List[Int] = Await.result(allUpTimes3, 1.second)
  println(s"Result with out Traverse with mapN Combiner: ${res3.mkString("[", ", ", "]")}")

  /**
   * Using Traverse
   */
  val allUpTimesTraverse: Future[List[Int]] =
    hostNames.traverse(getUpTime)

  val resTraverse: List[Int] = Await.result(allUpTimesTraverse, 1.second)
  println(s"Result with Traverse: ${resTraverse.mkString("[", ", ", "]")}")

  /**
   * Using listTraverse
   */
  def listTraverse[F[_]: Applicative, A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldLeft(List.empty[B].pure[F]) { (acc: F[List[B]], a: A) =>
      (acc, f(a)).mapN(_ :+ _)
    }

  val allUpTimesListTraverse: Future[List[Int]] =
    listTraverse(hostNames)(getUpTime)

  val resListTraverse: List[Int] = Await.result(allUpTimesListTraverse, 1.second)
  println(s"Result with List Traverse: ${resListTraverse.mkString("[", ", ", "]")}")

  /**
   * List Sequence
   */
  val lvi: List[Vector[Int]] = List(Vector(1, 2, 3), Vector(4, 5), Vector(6, 7))
  val vli: Vector[List[Int]] = lvi.sequence

  println()
  println(vli)

  /**
   * Traverse over Option
   */
  def process(list: List[Int]): Option[List[Int]] =
    list.traverse(i => if (i % 2 == 0) Some(i) else None)

  val li1: List[Int] = List(1, 3, 5)
  val li2: List[Int] = List(2, 4, 6)
  val li3: List[Int] = List(1, 2, 3, 4, 5)

  println()
  println(process(li1))
  println(process(li2))
  println(process(li3))

  /**
   * Traverse with Validated
   */
  type ErrorsOr[A] = Validated[List[String], A]

  def processValidate(list: List[Int]): ErrorsOr[List[Int]] =
    list.traverse{ i =>
      if (i % 2 == 0) Validated.valid(i)
      else Validated.invalid(List(s"$i is not even"))
    }

  println()
  println(processValidate(li1))
  println(processValidate(li2))
  println(processValidate(li3))
}
