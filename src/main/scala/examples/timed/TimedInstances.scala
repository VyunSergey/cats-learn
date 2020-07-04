package examples.timed

object TimedInstances {
  implicit def allTimedInst[A]: Timed[A] = new Timed[A] {}
}
