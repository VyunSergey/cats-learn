package examples.testingasynccode.client

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class RealUpTimeClient extends UpTimeClient[Future] {
  // just for the example
  def getUpTime(hostName: String): Future[Int] = Future(1)
}
