package examples.testingasynccode

import cats.Id
import examples.testingasynccode.client.TestUpTimeClient
import examples.testingasynccode.service.UpTimeService

object UpTimeTest extends App {
  def testTotalUpTime(hosts: Map[String, Int]): (Int, Int) = {
    val client = new TestUpTimeClient(hosts)
    val service = new UpTimeService[Id](client)
    val actual = service.getTotalUpTime(hosts.keys.toList)
    val expected = hosts.values.sum
    (actual, expected)
  }

  def printUpTime(actual: Int, expected: Int): Unit = {
    println(s"Actual TotalUpTime: $actual and Expected TotalUpTime: $expected")
  }

  def assertUpTime(actual: Int, expected: Int): Unit = {
    assert(actual == expected)
  }

  // Test1
  val hosts1 = Map("host1" -> 0, "host2" -> 0)
  val totals1 = testTotalUpTime(hosts1)
  printUpTime(totals1._1, totals1._2)
  assertUpTime(totals1._1, totals1._2)

  // Test2
  val hosts2 = Map("host1" -> -1, "host2" -> 1)
  val totals2 = testTotalUpTime(hosts2)
  printUpTime(totals2._1, totals2._2)
  assertUpTime(totals2._1, totals2._2)

  // Test3
  val hosts3 = Map("host1" -> 10, "host2" -> 23)
  val totals3 = testTotalUpTime(hosts3)
  printUpTime(totals3._1, totals3._2)
  assertUpTime(totals3._1, totals3._2)
}
