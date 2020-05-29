package examples.printable

import PrintableInstances._
import PrintableSyntax._

object PrintableTest extends App {
  val str: String = "Serge Wu"

  println(Printable.format(str))
  Printable.print(str)
  println(str.format(stringPrintable))
  str.print

  val cat = Cat("Margo", 11, "black")

  println(Printable.format(cat))
  Printable.print(cat)
  println(cat.format)
  cat.print
}
