package examples.testingasynccode.client

import cats.Id

class TestUpTimeClient(hosts: Map[String, Int]) extends UpTimeClient[Id] {
  // implement getUpTime using given hosts
  def getUpTime(hostName: String): Int =
    hosts.getOrElse(hostName, 0)
}
