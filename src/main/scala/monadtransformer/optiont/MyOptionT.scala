package monadtransformer.optiont

import cats._

/**
 * `OptionT[F[_], A]` is a light wrapper on an `F[Option[A]]` with some
 * convenient methods for working with this nested structure.
 *
 * It may also be said that `OptionT` is a monad transformer for `Option`.
 */
final case class MyOptionT[F[_], A](value: F[Option[A]]) {
  def fold[B](default: => B)(f: A => B)(implicit F: Functor[F]): F[B] =
    F.map(value)(_.fold(default)(f))

  def cata[B](default: => B, f: A => B)(implicit F: Functor[F]): F[B] =
    fold(default)(f)

  def map[B](f: A => B)(implicit F: Functor[F]): MyOptionT[F, B] =
    MyOptionT(F.map(value)(_.map(f)))

  def imap[B](f: A => B)(g: B => A)(implicit F: Invariant[F]): MyOptionT[F, B] =
    MyOptionT(F.imap(value)(_.map(f))(_.map(g)))

  def contramap[B](f: B => A)(implicit F: Contravariant[F]): MyOptionT[F, B] =
    MyOptionT(F.contramap(value)(_.map(f)))

  def mapK[G[_]](f: F ~> G): MyOptionT[G, A] =
    MyOptionT(f(value))

  def semiflatMap[B](f: A => F[B])(implicit F: Monad[F]): MyOptionT[F, B] =
    flatMap(a => MyOptionT(F.map(f(a))(Some(_))))

  def mapFilter[B](f: A => Option[B])(implicit F: Functor[F]): MyOptionT[F, B] =
    subflatMap(f)

  def flatMap[B](f: A => MyOptionT[F, B])(implicit F: Monad[F]): MyOptionT[F, B] =
    flatMapF(a => f(a).value)

  def flatMapF[B](f: A => F[Option[B]])(implicit F: Monad[F]): MyOptionT[F, B] =
    MyOptionT(F.flatMap(value)(_.fold(F.pure[Option[B]](None))(f)))

  def flatTransform[B](f: Option[A] => F[Option[B]])(implicit F: Monad[F]): MyOptionT[F, B] =
    MyOptionT(F.flatMap(value)(f))

  def transform[B](f: Option[A] => Option[B])(implicit F: Functor[F]): MyOptionT[F, B] =
    MyOptionT(F.map(value)(f))

  def subflatMap[B](f: A => Option[B])(implicit F: Functor[F]): MyOptionT[F, B] =
    MyOptionT(F.map(value)(_.flatMap(f)))

  def collect[B](f: PartialFunction[A, B])(implicit F: Functor[F]): MyOptionT[F, B] =
    MyOptionT(F.map(value)(_.collect(f)))

  def exists(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(value)(_.exists(f))

  def filter(p: A => Boolean)(implicit F: Functor[F]): MyOptionT[F, A] =
    MyOptionT(F.map(value)(_.filter(p)))

  def withFilter(p: A => Boolean)(implicit F: Functor[F]): MyOptionT[F, A] =
    filter(p)(F)

  def filterNot(p: A => Boolean)(implicit F: Functor[F]): MyOptionT[F, A] =
    MyOptionT(F.map(value)(_.filterNot(p)))

  def forall(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(value)(_.forall(f))

  def isDefined(implicit F: Functor[F]): F[Boolean] =
    F.map(value)(_.isDefined)

  def isEmpty(implicit F: Functor[F]): F[Boolean] =
    F.map(value)(_.isEmpty)

  def orElse(default: => MyOptionT[F, A])(implicit F: Monad[F]): MyOptionT[F, A] =
    orElseF(default.value)

  def orElseF(default: => F[Option[A]])(implicit F: Monad[F]): MyOptionT[F, A] =
    MyOptionT(F.flatMap(value)({
      case s @ Some(_) => F.pure(s)
      case None => default
    }))

  def show(implicit F: Show[F[Option[A]]]): String =
    F.show(value)

  def compare(that: MyOptionT[F, A])(implicit o: Order[F[Option[A]]]): Int =
    o.compare(value, that.value)

  def partialCompare(that: MyOptionT[F, A])(implicit p: PartialOrder[F[Option[A]]]): Double =
    p.partialCompare(value, that.value)

  def ===(that: MyOptionT[F, A])(implicit eq: Eq[F[Option[A]]]): Boolean =
    eq.eqv(value, that.value)

  def traverse[G[_], B](f: A => G[B])(implicit F: Traverse[F], G: Applicative[G]): G[MyOptionT[F, B]] =
    G.map(F.compose(Traverse[Option]).traverse(value)(f))(MyOptionT(_))

  def foldLeft[B](b: B)(f: (B, A) => B)(implicit F: Foldable[F]): B =
    F.compose(Foldable[Option]).foldLeft(value, b)(f)

  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B])(implicit F: Foldable[F]): Eval[B] =
    F.compose(Foldable[Option]).foldRight(value, lb)(f)
}
