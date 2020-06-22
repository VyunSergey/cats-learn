package monadtransformer.idt

import cats.Show
import cats.data.IdT
import cats.instances.list._
import cats.syntax.applicative._

object IdTTest extends App {
  type ListId[A] = IdT[List, A]

  implicit class IdTWithShow[F[_], A](idt: IdT[F, A]) {
    def show(implicit F: Show[F[A]]): String = F.show(idt.value)
  }

  val res1: ListId[Int] = IdT[List, Int](List(42))
  val res2: ListId[Int] = 42.pure[ListId]
  val res3: ListId[Int] = IdT.apply(List(1, 2, 3))
  val res4: ListId[Int] = IdT(res3.value ::: Nil ::: Nil)

  implicit val listIdShow: Show[List[Int]] = (li: List[Int]) =>
    s"result: ${li.map(_.toString).foldLeft("")(_ + "," + _).tail}"

  println(res1.show)
  println(res2.show)
  println(res3.show)
  println(res4.show)

  val res5 = for {
    a <- res1
    b <- res2
    c <- res3
    d <- res4
  } yield a - b + c + d

  println(res5.show)

}
