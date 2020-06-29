package foldable

import cats._

trait MyFoldable[F[_]] {self =>
  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B

  def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]

  def reduceLeftToOption[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): Option[B] =
    foldLeft(fa, Option.empty[B]) {(ob, a) =>
      ob match {
        case Some(b) => Some(g(b, a))
        case None => Some(f(a))
      }
    }

  def reduceRightToOption[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[Option[B]] =
    foldRight(fa, Now(Option.empty[B])) {(a, eob) =>
      eob.flatMap {
        case Some(b) => g(a, Now(b)).map(Some.apply)
        case None => Later(f(a)).map(Some.apply)
      }
    }

  def reduceLeftOption[A](fa: F[A])(f: (A, A) => A): Option[A] =
    reduceLeftToOption(fa)(identity)(f)

  def reduceRightOption[A](fa: F[A])(f: (A, Eval[A]) => Eval[A]): Eval[Option[A]] =
    reduceRightToOption(fa)(identity)(f)

  def fold[A](fa: F[A])(implicit A: Monoid[A]): A =
    A.combineAll(toIterable(fa)(self))

  def toIterable[G[_], A](ga: G[A])(G: MyFoldable[G]): Iterable[A] =
    G.foldRight[A, Stream[A]](ga, Eval.now(Stream.empty)) { (a, eb) =>
      eb.map(Stream.cons(a, _))
    }.value
}
