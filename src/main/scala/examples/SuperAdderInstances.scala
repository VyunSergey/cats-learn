package examples

import cats.Monoid

object SuperAdderInstances {
  val emptyOrder: Order = Order(0.0, 0.0)

  implicit val orderMonoid: Monoid[Order] =
    Monoid.instance[Order](emptyOrder,
      (ordA, ordB) => Order(ordA.totalCost + ordB.totalCost, ordA.quantity + ordB.quantity))

}
