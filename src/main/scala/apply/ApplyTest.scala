package apply

import cats.Eval
import cats.syntax.applicative._
import cats.syntax.apply._

object ApplyTest extends App {
  val li: List[Int] = List(123)
  val fl: List[Int => String] = List((i: Int) => i.toString)

  val ls1: List[String] = fl.ap(li)
  val ls2: List[String] = fl <*> li

  val lr1: List[Int] = fl.productR(li)
  val lr2: List[Int] = fl *> li

  val ll1: List[Int => String] = fl.productL(li)
  val ll2: List[Int => String] = fl <* li

  val lm1: List[Int] = li.map2(List(456))({case (i1, i2) => i1 + i2})
  val lm2: Eval[List[Int]] = li.map2Eval(List(456).pure[Eval])({case (i1, i2) => i1 + i2})

  println(ls1)
  println(ls2)
  println(lr1)
  println(lr2)
  println(ll1)
  println(ll2)
  println(lm1)
  println(lm2.value)
}
