package monad

import cats.{Monad, Id}
import cats.instances.list._
import cats.instances.option._
import cats.instances.future._
import cats.syntax.functor._
import cats.syntax.flatMap._

import scala.concurrent.{Future, Await}
import scala.concurrent.duration._

object MonadTest extends App {
  def sumSquared[F[_]: Monad](fa: F[Int], fb: F[Int]): F[Int] = for {
    a <- fa
    b <- fb
  } yield a * a + b * b

  val i1: Id[Int] = Monad[Id].pure(1)
  val i2: Id[Int] = Monad[Id].flatMap(i1)(x => x + 2)
  val i3: Id[Int] = Monad[Id].map(i2)(x => 100 * x)
  val i4: Id[Int] = sumSquared(i2, i3)

  println(s"Int op1: $i1")
  println(s"Int op2: $i2")
  println(s"Int op3: $i3")
  println(s"Int op4: $i4")
  println()

  val op1: Option[Int] = Monad[Option].pure(1)
  val op2: Option[Int] = Monad[Option].flatMap(op1)(x => Some(x + 2))
  val op3: Option[Int] = Monad[Option].map(op2)(x => 100 * x)
  val op4: Option[Int] = sumSquared(op2, op3)

  println(s"Option op1: $op1")
  println(s"Option op2: $op2")
  println(s"Option op3: $op3")
  println(s"Option op4: $op4")
  println()

  val lst1: List[Int] = Monad[List].pure(1)
  val lst2: List[Int] = Monad[List].flatMap(lst1)(x => List(x + 2))
  val lst3: List[Int] = Monad[List].map(lst2)(x => 100 * x)
  val lst4: List[Int] = sumSquared(lst2, lst3)

  println(s"List lst1: $lst1")
  println(s"List lst2: $lst2")
  println(s"List lst3: $lst3")
  println(s"List lst4: $lst4")
  println()

  import scala.concurrent.ExecutionContext.Implicits.global

  val fr1: Future[Int] = Monad[Future].pure(1)
  val fr2: Future[Int] = Monad[Future].flatMap(fr1)(x => Future(x + 2))
  val fr3: Future[Int] = Monad[Future].map(fr2)(x => 100 * x)
  val fr4: Future[Int] = sumSquared(fr2, fr3)

  def toResult[A](f: Future[A])(implicit d: Duration): A = Await.result(f, d)
  implicit val d: Duration = 10.seconds

  println(s"Future fr1: ${toResult(fr1)}")
  println(s"Future fr2: ${toResult(fr2)}")
  println(s"Future fr3: ${toResult(fr3)}")
  println(s"Future fr4: ${toResult(fr4)}")
  println()

}
