package contravariant

trait MyContravariant[F[_]] {
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
}

object MyContravariant {
  def apply[F[_]](implicit inst: MyContravariant[F]): MyContravariant[F] = inst
}
