package monadtransformer.readert

import cats._

/*
 * Represents a function `A => F[B]`.
 */
final case class MyReaderT[F[_], A, B](run: A => F[B]) {
  def ap[C, AA <: A](f: MyReaderT[F, AA, B => C])(implicit F: Apply[F]): MyReaderT[F, AA, C] =
    MyReaderT(a => F.ap(f.run(a))(run(a)))

  def dimap[C, D](f: C => A)(g: B => D)(implicit F: Functor[F]): MyReaderT[F, C, D] =
    MyReaderT(c => F.map(run(f(c)))(g))

  def map[C](f: B => C)(implicit F: Functor[F]): MyReaderT[F, A, C] =
    dimap(identity[A])(f)

  def mapF[N[_], C](f: F[B] => N[C]): MyReaderT[N, A, C] =
    MyReaderT(a => f(run(a)))

  def mapK[G[_]](f: F ~> G): MyReaderT[G, A, B] =
    MyReaderT(a => f(run(a)))

  def flatMap[C, AA <: A](f: B => MyReaderT[F, AA, C])(implicit F: FlatMap[F]): MyReaderT[F, AA, C] =
    MyReaderT(a => F.flatMap(run(a))(b => f(b).run(a)))

  def flatMapF[C](f: B => F[C])(implicit F: FlatMap[F]): MyReaderT[F, A, C] =
    MyReaderT(a => F.flatMap(run(a))(b => f(b)))

  def lift[G[_]](implicit G: Applicative[G]): MyReaderT[λ[α => G[F[α]]], A, B] =
    MyReaderT[λ[α => G[F[α]]], A, B](a => G.pure(run(a)))

  def liftF[G[_], C, D](x: G[D]): MyReaderT[G, C, D] =
    MyReaderT(_ => x)

  def liftK[G[_], C]: G ~> MyReaderT[G, C, *] =
    new (G ~> MyReaderT[G, C, *]) { def apply[D](fb: G[D]): MyReaderT[G, C, D] = liftF[G, C, D](fb) }

  def local[AA](f: AA => F[A])(implicit F: FlatMap[F]): MyReaderT[F, AA, B] =
    MyReaderT(aa => F.flatMap(f(aa))(run))

  def andThen[C](f: B => F[C])(implicit F: FlatMap[F]): MyReaderT[F, A, C] =
    MyReaderT(a => F.flatMap(run(a))(f))

  def compose[AA](f: AA => F[A])(implicit F: FlatMap[F]): MyReaderT[F, AA, B] =
    local(f)

  def apply(a: A): F[B] = run(a)

  def traverse[G[_], AA <: A](f: G[AA])(implicit F: Applicative[F], G: Traverse[G]): F[G[B]] =
    G.traverse(f)(run)
}
