package examples.timed

trait Timed[A] {
  def time(block: => A): A = {
    // time start
    val startTime: Long = System.nanoTime()

    // call-by-name
    val result = block

    // time finish
    val finishTime: Long = System.nanoTime()

    val timeNs: Double = (finishTime - startTime).toDouble
    val timeS: Double = timeNs / 1000000000.0
    val timeM: Double = timeS / 60.0
    val timeH: Double = timeM / 60.0

    val timeFloorH: Int = timeH.floor.toInt
    val timeFloorM: Int = (timeM - timeFloorH * 60.0).floor.toInt
    val timeFloorS: Double = timeS - (timeFloorH * 60.0 + timeFloorM) * 60.0

    val timeStrH: List[String] = if (timeH > 1.0) List(timeFloorH + "hour") else Nil
    val timeStrM: List[String] = if (timeM > 1.0) List(timeFloorM + "min") else Nil
    val timeStrS: List[String] = List(timeFloorS.formatted("%f") + "sec")
    val timeList: List[String] = timeStrH ++ timeStrM ++ timeStrS

    println(s"Elapsed time: ${timeList.mkString(" ")}(${timeNs.toLong}ns)")
    result
  }
}

object Timed {
  def apply[A](implicit instance: Timed[A]): Timed[A] = instance
}
