package validated

import cats._
import cats.data.Validated.Valid
import cats.data.{Validated, ValidatedNec, ValidatedNel}
import validated.MyValidated.{MyInvalid, MyValid}

sealed abstract class MyValidated[+E, +A] extends Product with Serializable {
  def fold[B](fe: E => B, fa: A => B): B =
    this match {
      case MyValid(a) => fa(a)
      case MyInvalid(e) => fe(e)
    }

  def isValid: Boolean = fold(_ => false, _ => true)

  def isInvalid: Boolean = fold(_ => true, _ => false)

  def foreach(f: A => Unit): Unit = fold(_ => (), a => f(a))

  def getOrElse[B >: A](default: => B): B = fold(_ => default, identity)

  def valueOr[B >: A](f: E => B): B = fold(e => f(e), identity)

  def exists(predicate: A => Boolean): Boolean = fold(_ => false, a => predicate(a))

  def forall(f: A => Boolean): Boolean = fold(_ => false, a => f(a))

  def orElse[EE, AA >: A](default: => MyValidated[EE, AA]): MyValidated[EE, AA] =
    this match {
      case MyValid(a) => MyValid(a)
      case _ => default
    }

  def findValid[EE >: E, AA >: A](that: => MyValidated[EE, AA])
                                 (implicit EE: Semigroup[EE]): MyValidated[EE, AA] =
    this match {
      case MyValid(a) => MyValid(a)
      case MyInvalid(e) =>
        that match {
          case MyValid(aa) => MyValid(aa)
          case MyInvalid(ee) => MyInvalid(EE.combine(e, ee))
        }
    }

  def toEither: Either[E, A] = fold(e => Left(e), a => Right(a))

  def toOption: Option[A] = fold(_ => None, a => Some(a))

  def toList: List[A] = fold(_ => Nil, a => List(a))

  def toValidatedNel[EE >: E, AA >: A]: ValidatedNel[EE, AA] =
    this match {
      case MyValid(a) => Valid(a)
      case MyInvalid(e) => Validated.invalidNel(e)
    }

  def toValidatedNec[EE >: E, AA >: A]: ValidatedNec[EE, AA] =
    this match {
      case MyValid(a) => Valid(a)
      case MyInvalid(e) => Validated.invalidNec(e)
    }

  def withEither[EE, B](f: Either[E, A] => Either[EE, B]): MyValidated[EE, B] =
    f(this.toEither) match {
      case Right(a) => MyValid(a)
      case Left(e) => MyInvalid(e)
    }

  def compare[EE >: E, AA >: A](that: MyValidated[EE, AA])
                               (implicit EE: Order[EE], AA: Order[AA]): Int =
    this.fold2(that)(EE.compare, AA.compare, (_, _) => -1, (_, _) => 1)

  def partialCompare[EE >: E, AA >: A](that: MyValidated[EE, AA])
                                      (implicit EE: PartialOrder[EE], AA: PartialOrder[AA]): Double =
    this.fold2(that)(EE.partialCompare, AA.partialCompare, (_, _) => -1, (_, _) => 1)

  def show[EE >: E, AA >: A](implicit EE: Show[EE], AA: Show[AA]): String =
    fold(e => s"Invalid(${EE.show(e)})", a => s"Valid(${AA.show(a)})")

  def ===[EE >: E, AA >: A](that: MyValidated[EE, AA])
                           (implicit EE: Eq[EE], AA: Eq[AA]): Boolean =
    this.fold2(that)(EE.eqv, AA.eqv, (_, _) => false, (_, _) => false)

  def ap[EE >: E, B](f: MyValidated[EE, A => B])
                    (implicit EE: Semigroup[EE]): MyValidated[EE, B] =
    this.map2(f)((a, f) => f(a))

  def product[EE >: E, B](fb: MyValidated[EE, B])
                         (implicit EE: Semigroup[EE]): MyValidated[EE, (A, B)] =
    this.map2(fb)((a, b) => (a, b))

  def map[B](f: A => B): MyValidated[E, B] =
    this match {
      case MyValid(a) => MyValid(f(a))
      case MyInvalid(e) => MyInvalid(e)
    }

  def map2[EE >: E, B, C](that: MyValidated[EE, B])(f: (A, B) => C)
                         (implicit EE: Semigroup[EE]): MyValidated[EE, C] =
    (this, that) match {
      case (MyValid(a), MyValid(b)) => MyValid(f(a, b))
      case (MyValid(_), MyInvalid(ee)) => MyInvalid(ee)
      case (MyInvalid(e), MyValid(_)) => MyInvalid(e)
      case (MyInvalid(e), MyInvalid(ee)) => MyInvalid(EE.combine(e, ee))
    }

  def fold2[EE >: E, AA >: A, B](that: MyValidated[EE, AA])
                                (fe: (EE, EE) => B, fa: (AA, AA) => B, fea: (EE, AA) => B, fae: (AA, EE) => B): B =
    (this, that) match {
      case (MyValid(a), MyValid(aa)) => fa(a, aa)
      case (MyValid(a), MyInvalid(ee)) => fae(a, ee)
      case (MyInvalid(e), MyValid(aa)) => fea(e, aa)
      case (MyInvalid(e), MyInvalid(ee)) => fe(e, ee)
    }

  def leftMap[EE](f: E => EE): MyValidated[EE, A] =
    this match {
      case MyValid(a) => MyValid(a)
      case MyInvalid(e) => MyInvalid(f(e))
    }

  def traverse[F[_], EE >: E, B](f: A => F[B])
                                (implicit F: Applicative[F]): F[MyValidated[EE, B]] =
    this match {
      case MyValid(a) => F.map(f(a))(MyValid(_))
      case MyInvalid(e) => F.pure(MyInvalid(e))
    }

  def foldLeft[B](b: B)(f: (B, A) => B): B =
    this match {
      case MyValid(a) => f(b, a)
      case MyInvalid(_) => b
    }

  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    this match {
      case MyValid(a) => f(a, lb)
      case MyInvalid(_) => lb
    }

  def andThen[EE >: E, B](f: A => MyValidated[EE, B]): MyValidated[EE, B] =
    this match {
      case MyValid(a) => f(a)
      case MyInvalid(e) => MyInvalid(e)
    }

  def combine[EE >: E, AA >: A](that: MyValidated[EE, AA])
                               (implicit EE: Semigroup[EE], AA: Semigroup[AA]): MyValidated[EE, AA] =
    this.map2(that)(AA.combine)

  def swap: MyValidated[A, E] =
    this match {
      case MyValid(a) => MyInvalid(a)
      case MyInvalid(e) => MyValid(e)
    }

  def merge[EE >: E](implicit ev: A <:< EE): EE =
    this match {
      case MyValid(a) => ev(a)
      case MyInvalid(e) => e
    }

  def ensure[EE >: E](onFailure: => EE)(f: A => Boolean): MyValidated[EE, A] =
    this match {
      case MyValid(a) => if (f(a)) MyValid(a) else MyInvalid(onFailure)
      case MyInvalid(e) => MyInvalid(e)
    }

  def ensureOr[EE >: E](onFailure: A => EE)(f: A => Boolean): MyValidated[EE, A] =
    this match {
      case MyValid(a) => if (f(a)) MyValid(a) else MyInvalid(onFailure(a))
      case MyInvalid(e) => MyInvalid(e)
    }
}

object MyValidated {
  final case class MyValid[A](a: A) extends MyValidated[Nothing, A]

  final case class MyInvalid[E](e: E) extends MyValidated[E, Nothing]
}
