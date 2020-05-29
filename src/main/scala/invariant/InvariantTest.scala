package invariant

import cats.{Monoid, Invariant}
import cats.instances.string._
import cats.syntax.invariant._
import cats.syntax.monoid._

object InvariantTest extends App {
  val strMonoid = Monoid[String]
  implicit val symbolMonoid: Monoid[Symbol] =
    Invariant[Monoid].imap(strMonoid)(Symbol.apply)(_.name)

  val symbolMonoid2: Monoid[Symbol] =
    strMonoid.imap[Symbol]((s: String) => Symbol(s))((sym: Symbol) => sym.name)

  println(Monoid[Symbol].empty)
  println(Symbol("Scala") |+| Symbol(" with") |+| Symbol(" Cats!"))
  println()

  println(symbolMonoid2.empty)
  println(symbolMonoid2.combine(symbolMonoid2.combine(Symbol("Scala"), Symbol(" with")), Symbol(" Cats!")))

}
