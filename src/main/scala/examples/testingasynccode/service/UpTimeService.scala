package examples.testingasynccode.service

import cats.Applicative
import cats.syntax.functor._
import cats.syntax.traverse._
import examples.testingasynccode.client.UpTimeClient

class UpTimeService[F[_]: Applicative](client: UpTimeClient[F]) {
  def getTotalUpTime(hostNames: List[String]): F[Int] =
    hostNames.traverse(client.getUpTime).map(_.sum)
}
