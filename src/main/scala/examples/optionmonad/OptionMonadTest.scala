package examples.optionmonad

import cats.syntax.applicative._

object OptionMonadTest extends App {
  import OptionMonadInstances._

  val program = for {
    a <- 123.pure[Option]
    b = a + 1
    c <- 345.pure[Option]
    d = c - b
    e <- 321.pure[Option]
    f = e - d
    g <- 543.pure[Option]
    h = g - f
    k <- 444.pure[Option]
    l = k - h
  } yield l

  println(program)

}
