package foldable

import cats._
import cats.instances.int._
import cats.instances.list._
import cats.instances.option._
import cats.instances.stream._
import cats.instances.string._
import cats.syntax.foldable._

object FoldableTest extends App {
  val l: List[Int] = List(1, 2, 3)

  println(Foldable[List].combineAll(l))
  println(Foldable[List].foldMap(l)(_.toString))

  println(l.combineAll)
  println(l.foldMap(_.toString))

  val lfl: Int = Foldable[List].foldLeft(l, 0)(_ + _)
  val lfr: Eval[Int] = Foldable[List].foldRight(l, Now(0)) {
    case (i: Int, ei: Eval[Int]) => ei.map(_ + i)
  }

  println(lfl)
  println(lfr.value)

  val o: Option[Int] = Some(123)

  val ofl: Int = Foldable[Option].foldLeft(o, 1)(_ * _)
  val ofr: Eval[Int] = Foldable[Option].foldRight(o, Now(1)) {
    case (i: Int, ei: Eval[Int]) => ei.map(_ * i)
  }

  println(ofl)
  println(ofr.value)

  val bigData: Stream[Int] = (1 to 1000000).toStream

  // Stream doesn`t provide stack safe foldRight
  val bdFr: Long =
    try {
      bigData.foldRight(0L)(_ + _)
    } catch {
      case _: Throwable => 0L
    }

  val bdFrCats: Eval[Long] = Foldable[Stream].foldRight(bigData, Now(0L)) {
    case (i: Int, el: Eval[Long]) => el.map(_ + i)
  }

  println(bdFr)
  println(bdFrCats.value)
  // List and Vector provide stack safe foldRight
  println(bigData.toList.foldRight(0L)(_ + _))
  println(bigData.toVector.foldRight(0L)(_ + _))
}
