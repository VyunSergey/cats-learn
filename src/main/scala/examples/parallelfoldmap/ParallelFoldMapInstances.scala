package examples.parallelfoldmap

import cats.instances.future._
import cats.syntax.monoid._
import cats.{Monoid, Traverse}
import traverse.MyTraverseInstances._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object ParallelFoldMapInstances {
  implicit def traverseParallelFoldMapInst[F[_] <: Iterable[_]]: ParallelFoldMap[F] =
    new ParallelFoldMap[F] {
      def foldMap[A, B](fa: F[A])(f: A => B)(implicit B: Monoid[B]): Future[B] = {
        // convert F to Iterable
        val itrA: Iterable[A] = fa.asInstanceOf[Iterable[A]]

        // get number of cores for CRU
        val numCores: Int = Runtime.getRuntime.availableProcessors

        // calculate number of element is each group
        val groupSize: Int = (1.0 * itrA.size / numCores).ceil.toInt

        // create group for parallel foldMap
        val groups: Iterator[Iterable[A]] = itrA.grouped(groupSize)

        // map groups in parallel using Future
        val mapGroups: Iterable[Future[B]] = groups.map {
          group => Future(group.foldLeft(B.empty)((b, a) => b |+| f(a)))
        }.toIterable

        // fold groups in parallel using Traverse and Future
        val foldGroups: Future[B] = Traverse[Iterable].sequence(mapGroups).map {
          iterable => iterable.foldLeft(B.empty)(_ |+| _)
        }

        foldGroups
      }
    }
}
