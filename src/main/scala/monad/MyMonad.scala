package monad

trait MyMonad[F[_]] {
  def pure[A](value: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def map[A, B](fa: F[A])(f: A => B): F[B] = this.flatMap(fa)(this.pure[B] _ compose f)
}

object MyMonad {
  def apply[F[_]](implicit m: MyMonad[F]): MyMonad[F] = m
}
