package monadtransformer.eithert

import cats._

/**
 * `EitherT[F[_], A, B]` is a light wrapper on an `F[Either[A, B]]` with some
 * convenient methods for working with this nested structure.
 *
 * It may also be said that `EitherT` is a monad transformer for `Either`.
 */
final case class MyEitherT[F[_], A, B](value: F[Either[A, B]]) {
  def applyAlt[D](ff: MyEitherT[F, A, B => D])(implicit F: Apply[F]): MyEitherT[F, A, D] =
    MyEitherT(F.map2(value, ff.value)((xb, xbd) => Apply[Either[A, *]].ap(xbd)(xb)))

  def fold[C](fa: A => C, fb: B => C)(implicit F: Functor[F]): F[C] =
    F.map(value)(_.fold(fa, fb))

  def foldF[C](fa: A => F[C], fb: B => F[C])(implicit F: FlatMap[F]): F[C] =
    F.flatMap(value)(_.fold(fa, fb))

  def forall(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(value)(_.forall(f))

  def map[D](f: B => D)(implicit F: Functor[F]): MyEitherT[F, A, D] =
    MyEitherT(F.map(value)(_.map(f)))

  def mapK[G[_]](f: ~>[F, G]): MyEitherT[G, A, B] =
    MyEitherT(f(value))

  def bimap[C, D](fa: A => C, fb: B => D)(implicit F: Functor[F]): MyEitherT[F, C, D] =
    MyEitherT(F.map(value)({
      case Left(a) => Left(fa(a))
      case Right(b) => Right(fb(b))
    }))

  def biflatMap[C, D](fa: A => MyEitherT[F, C, D], fb: B => MyEitherT[F, C, D])(implicit F: FlatMap[F]): MyEitherT[F, C, D] =
    MyEitherT(F.flatMap(value)({
      case Left(a) => fa(a).value
      case Right(b) => fb(b).value
    }))

  def flatMap[AA >: A, D](f: B => MyEitherT[F, AA, D])(implicit F: Monad[F]): MyEitherT[F, AA, D] =
    MyEitherT(F.flatMap(value)({
      case Left(a) => F.pure(Left(a))
      case Right(b) => f(b).value
    }))

  def flatMapF[AA >: A, D](f: B => F[Either[AA, D]])(implicit F: Monad[F]): MyEitherT[F, AA, D] =
    flatMap(b => MyEitherT(f(b)))

  def semiflatMap[D](f: B => F[D])(implicit F: Monad[F]): MyEitherT[F, A, D] =
    MyEitherT(F.flatMap(value)({
      case Left(a) => F.pure(Left(a))
      case Right(b) => F.map(f(b))(Right(_))
    }))

  def biSemiflatMap[C, D](fa: A => F[C], fb: B => F[D])(implicit F: Monad[F]): MyEitherT[F, C, D] =
    MyEitherT(F.flatMap(value)({
      case Left(a) => F.map(fa(a))(Left(_))
      case Right(b) => F.map(fb(b))(Right(_))
    }))

  def leftSemiflatMap[D](f: A => F[D])(implicit F: Monad[F]): MyEitherT[F, D, B] =
    MyEitherT(F.flatMap(value)({
      case Left(a) => F.map(f(a))(Left(_))
      case Right(b) => F.pure(Right(b))
    }))

  def subflatMap[AA >: A, D](f: B => Either[AA, D])(implicit F: Functor[F]): MyEitherT[F, AA, D] =
    MyEitherT(F.map(value)(_.flatMap(f)))

  def show(implicit F: Show[F[Either[A, B]]]): String =
    F.show(value)

  def compare(that: MyEitherT[F, A, B])(implicit o: Order[F[Either[A, B]]]): Int =
    o.compare(value, that.value)

  def partialCompare(that: MyEitherT[F, A, B])(implicit p: PartialOrder[F[Either[A, B]]]): Double =
    p.partialCompare(value, that.value)

  def ===(that: MyEitherT[F, A, B])(implicit eq: Eq[F[Either[A, B]]]): Boolean =
    eq.eqv(value, that.value)

  def combine(that: MyEitherT[F, A, B])(implicit F: Apply[F], B: Semigroup[B]): MyEitherT[F, A, B] =
    MyEitherT(F.map2(value, that.value)({
      case (Right(b1), Right(b2)) => Right(B.combine(b1, b2))
      case (Left(a), _) => Left(a)
      case (_, Left(a)) => Left(a)
    }))

  def transform[C, D](f: Either[A, B] => Either[C, D])(implicit F: Functor[F]): MyEitherT[F, C, D] =
    MyEitherT(F.map(value)(f))

  def traverse[G[_], D](f: B => G[D])(implicit F: Traverse[F], G: Applicative[G]): G[MyEitherT[F, A, D]] =
    G.map(F.compose(Traverse[Either[A, *]]).traverse(value)(f))(MyEitherT(_))

  def bitraverse[G[_], C, D](f: A => G[C], g: B => G[D])(implicit F: Traverse[F], G: Applicative[G]): G[MyEitherT[F, C, D]] =
    G.map(F.traverse(value)(axb => Bitraverse[Either].bitraverse(axb)(f, g)))(MyEitherT(_))

  def foldLeft[C](c: C)(f: (C, B) => C)(implicit F: Foldable[F]): C =
    F.foldLeft(value, c)({
      case (c, Right(b)) => f(c, b)
      case (c, Left(_)) => c
    })

  def foldRight[C](lc: Eval[C])(f: (B, Eval[C]) => Eval[C])(implicit F: Foldable[F]): Eval[C] =
    F.foldRight(value, lc)({
      case (Right(b), lc) => f(b, lc)
      case (Left(_), lc) => lc
    })
}
