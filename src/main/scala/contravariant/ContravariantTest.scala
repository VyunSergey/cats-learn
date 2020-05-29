package contravariant

import cats.Contravariant
import cats.Show
import cats.instances.string._
import cats.syntax.contravariant._
import cats.syntax.show._

object ContravariantTest extends App {
  val strShow: Show[String] = Show[String]
  val symbolShow: Show[Symbol] = Contravariant[Show].contramap(strShow)(sym => s"'${sym.name}")

  val s = "Scala with Cats!"
  val sym = Symbol(s)

  println(show"$s")
  println(strShow.show(s))
  println(show"$sym")
  println(symbolShow.show(sym))
  println(strShow.contramap[Symbol](sym => s"'${sym.name}").show(sym))

}
