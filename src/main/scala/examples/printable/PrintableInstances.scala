package examples.printable

object PrintableInstances {
  implicit val stringPrintable: Printable[String] =
    (str: String) => str

  implicit val integerPrintable: Printable[Int] =
    (i: Int) => i.toString

  implicit val catPrintable: Printable[Cat] =
    (c: Cat) => s"${c.name} is a ${c.age} year-old ${c.color} cat"

}
