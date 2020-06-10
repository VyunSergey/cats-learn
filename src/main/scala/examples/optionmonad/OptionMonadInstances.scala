package examples.optionmonad

import cats.Monad

import scala.annotation.tailrec

object OptionMonadInstances {
  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    /*
     * define `pure` method to lift a value x: A in Option
     * */
    def pure[A](x: A): Option[A] = Option(x)
    /*
     * define `flatMap` method for sequence computations
     * */
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    /*
     * define `tailRecM` method for optimisation used in Cats
     * to limit the amount of stack space consumed by nested calls to flatMap
     * */
    @tailrec
    def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case Some(Left(la)) => tailRecM(la)(f)
      case Some(Right(rb)) => Some(rb)
      case None => None
    }
  }

}
