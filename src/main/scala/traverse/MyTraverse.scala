package traverse

import cats._
import cats.data.{State, StateT}

trait MyTraverse[F[_]] { self =>
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def flatTraverse[G[_], A, B](fa: F[A])(f: A => G[F[B]])
                              (implicit G: Applicative[G], F: FlatMap[F]): G[F[B]] =
    G.map(traverse(fa)(f))(F.flatten)

  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(identity)

  def flatSequence[G[_], A](fgfa: F[G[F[A]]])
                           (implicit G: Applicative[G], F: FlatMap[F]): G[F[A]] =
    G.map(sequence(fgfa))(F.flatten)

  def compose[G[_]: MyTraverse]: MyTraverse[λ[α => F[G[α]]]] =
    new MyComposedTraverse[F, G] {
      val F: MyTraverse[F] = self
      val G: MyTraverse[G] = MyTraverse[G]
    }

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)

  def mapWithIndex[A, B](fa: F[A])(f: (A, Int) => B): F[B] =
    traverse(fa)(a => State((s: Int) => (s + 1, f(a, s)))).runA(0).value

  def traverseWithIndexM[G[_], A, B](fa: F[A])(f: (A, Int) => G[B])(implicit G: Monad[G]): G[F[B]] =
    traverse(fa)(a => StateT((s: Int) => G.map(f(a, s))(b => (s + 1, b)))).runA(0)

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapWithIndex(fa)((a, i) => (a, i))
}

object MyTraverse {
  def apply[F[_]](implicit F: MyTraverse[F]): MyTraverse[F] = F
}
