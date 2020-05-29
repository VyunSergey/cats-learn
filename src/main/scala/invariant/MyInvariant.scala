package invariant

trait MyInvariant[F[_]] {
  def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]
}

object MyInvariant {
  def apply[F[_]](implicit inst: MyInvariant[F]): MyInvariant[F] = inst
}
