package monad

import cats.Id

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object MyMonadInstances {
  implicit val idMonad: MyMonad[Id] = new MyMonad[Id] {
    def pure[A](value: A): Id[A] = value
    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
  }

  implicit val listMonad: MyMonad[List] = new MyMonad[List] {
    def pure[A](value: A): List[A] = List(value)
    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
  }

  implicit val optionMonad: MyMonad[Option] = new MyMonad[Option] {
    def pure[A](value: A): Option[A] = Option(value)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  }

  implicit val futureMonad: MyMonad[Future] = new MyMonad[Future] {
    def pure[A](value: A): Future[A] = Future(value)
    def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f)
  }

}
