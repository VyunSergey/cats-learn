package equal

import java.util.Date

import cats.Eq
import cats.instances.all._
import cats.syntax.eq._
import examples.printable.Cat

object EqInstances {
  implicit val dateEq: Eq[Date] = Eq.instance[Date]{
    (d1: Date, d2: Date) => d1.getTime === d2.getTime
  }

  implicit val catEq: Eq[Cat] = Eq.instance[Cat]{
    (c1: Cat, c2: Cat) => c1.name === c2.name && c1.age === c2.age && c1.color === c2.color
  }
}
