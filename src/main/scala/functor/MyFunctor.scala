package functor

trait MyFunctor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object MyFunctor {
  def apply[F[_]](implicit f: MyFunctor[F]): MyFunctor[F] = f
}
