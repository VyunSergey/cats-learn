package show

import cats.Show
import java.util.Date
import examples.printable.Cat

object ShowInstances {
  implicit val dateShow: Show[Date] =
    (t: Date) => s"${t.getTime}ms since the epoch."

  implicit val catPrintable: Show[Cat] =
    (c: Cat) => s"${c.name} is a ${c.age} year-old ${c.color} cat"
}
