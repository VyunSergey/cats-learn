package monaderror

import monad.MyMonad

/**
 * F is the effect with Monad instance
 * E is the type of error contained within F
 * */
trait MyMonadError[F[_], E] extends MyMonad[F] {
  // Lift an error into the `F` context
  def raiseError[A](e: E): F[A]

  // Handle an error, potentially recovering from it
  def handleError[A](fa: F[A])(f: E => A): F[A]

  // Testing an instance of `F`
  // failing with error `e` if the predicate is not satisfied
  def ensure[A](fa: F[A])(e: E)(f: A => Boolean): F[A]
}
