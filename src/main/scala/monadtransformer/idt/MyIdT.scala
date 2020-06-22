package monadtransformer.idt

import cats._

/**
 * `IdT[F[_], A]` is a light wrapper on an `F[Id[A]]` == `F[A]`
 *
 * It may also be said that `IdT` is the identity monad transformer.
 */
final case class MyIdT[F[_], A](value: F[A]) {
  def ap[B](f: MyIdT[F, A => B])(implicit F: Apply[F]): MyIdT[F, B] =
    MyIdT(F.ap(f.value)(value))

  def map[B](f: A => B)(implicit F: Functor[F]): MyIdT[F, B] =
    MyIdT(F.map(value)(f))

  def mapK[G[_]](f: F ~> G): MyIdT[G, A] =
    MyIdT(f(value))

  def flatMap[B](f: A => MyIdT[F, B])(implicit F: Monad[F]): MyIdT[F, B] =
    flatMapF(a => f(a).value)

  def flatMapF[B](f: A => F[B])(implicit F: Monad[F]): MyIdT[F, B] =
    MyIdT(F.flatMap(value)(f))

  def reduceLeftTo[B](f: A => B)(g: (B, A) ⇒ B)(implicit F: Reducible[F]): B =
    F.compose(Reducible[Id]).reduceLeftTo(value)(f)(g)

  def reduceRightTo[B](f: A => B)(g: (A, Eval[B]) ⇒ Eval[B])(implicit F: Reducible[F]): Eval[B] =
    F.compose(Reducible[Id]).reduceRightTo(value)(f)(g)

  def show(implicit F: Show[F[A]]): String =
    F.show(value)

  def compare(that: MyIdT[F, A])(implicit o: Order[F[A]]): Int =
    o.compare(value, that.value)

  def partialCompare(that: MyIdT[F, A])(implicit p: PartialOrder[F[A]]): Double =
    p.partialCompare(value, that.value)

  def ===(that: MyIdT[F, A])(implicit eq: Eq[F[A]]): Boolean =
    eq.eqv(value, that.value)

  def traverse[G[_], B](f: A => G[B])(implicit F: Traverse[F], G: Applicative[G]): G[MyIdT[F, B]] =
    G.map(F.compose(Traverse[Id]).traverse(value)(f))(MyIdT(_))

  def foldLeft[B](b: B)(f: (B, A) => B)(implicit F: Foldable[F]): B =
    F.compose(Foldable[Id]).foldLeft(value, b)(f)

  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B])(implicit F: Foldable[F]): Eval[B] =
    F.compose(Foldable[Id]).foldRight(value, lb)(f)
}
