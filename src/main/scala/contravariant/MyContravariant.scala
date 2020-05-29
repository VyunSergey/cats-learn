package contravariant

trait MyContravariant[F[_]] {
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
}

object MyContravariant {
  def apply[A](implicit inst: MyContravariant[A]): MyContravariant[A] = inst
}
