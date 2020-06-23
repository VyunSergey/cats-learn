package semigroupal

trait MySemigroupal[F[_]] {
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}

object MySemigroupal {
  def apply[F[_]](implicit s: MySemigroupal[F]): MySemigroupal[F] = s
}
