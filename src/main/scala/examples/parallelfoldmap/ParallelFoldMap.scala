package examples.parallelfoldmap

import cats.Monoid

import scala.concurrent.Future

trait ParallelFoldMap[F[_]] {
  def foldMap[A, B](fa: F[A])(f: A => B)(implicit B: Monoid[B]): Future[B]
}

object ParallelFoldMap {
  def apply[F[_]](implicit instance: ParallelFoldMap[F]): ParallelFoldMap[F] = instance
}
