package traverse

import cats._

trait MyComposedTraverse[F[_], G[_]] extends MyTraverse[λ[α => F[G[α]]]] {
  def F: MyTraverse[F]
  def G: MyTraverse[G]

  def traverse[H[_]: Applicative, A, B](fga: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
    F.traverse(fga)(ga => G.traverse(ga)(f))
}
