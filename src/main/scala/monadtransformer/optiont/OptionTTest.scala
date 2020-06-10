package monadtransformer.optiont

import cats.Show
import cats.data.OptionT
import cats.instances.list._
import cats.syntax.applicative._

object OptionTTest extends App {
  type ListOption[A] = OptionT[List, A]

  val res1: ListOption[Int] = OptionT[List, Int](List(Option(42)))
  val res2: ListOption[Int] = 42.pure[ListOption]
  val res3: ListOption[Int] = OptionT.fromOption(Some(42))
  val res4: ListOption[Int] = OptionT.liftF(List(1, 2, 3))
  val res5: ListOption[Int] = OptionT(res4.value ::: List(Option.empty[Int], Option.empty[Int]))

  implicit val listOptionShow: Show[List[Option[Int]]] = (loi: List[Option[Int]]) =>
    s"result: ${loi.map(_.map(_.toString).getOrElse("NULL")).foldLeft("")(_ + "," + _).tail}"

  println(res1.show)
  println(res2.show)
  println(res3.show)
  println(res4.show)
  println(res5.show)

  val res6 = for {
    a <- res1
    b <- res2
    c <- res3
    d <- res4
    e <- res5
  } yield a - b + c + d - e

  println(res6.show)

}
