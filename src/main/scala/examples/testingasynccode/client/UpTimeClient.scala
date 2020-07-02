package examples.testingasynccode.client

trait UpTimeClient[F[_]] {
  def getUpTime(hostName: String): F[Int]
}
