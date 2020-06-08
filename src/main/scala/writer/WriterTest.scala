package writer

import cats.data.{Writer, WriterT}
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._
import cats.{Id, Show}

object WriterTest extends App {
  val vecWriter: WriterT[Id, Vector[String], Int] =
    Writer(Vector("It was the best of times", "It was the worst of times"), 1859)
  println("Value: " + vecWriter.value)
  println("Log: " + vecWriter.written)
  println()

  implicit val showWriter: Show[(Vector[String], Int)] =
    Show.show((tp: (Vector[String], Int)) => s"Value: ${tp._2} with Log: ${tp._1.mkString("[\"", "\", \"", "\"]")}\n")
  println(vecWriter.show)

  type Logged[A] = Writer[Vector[String], A]

  val intLogged: Logged[Int] = 123.pure[Logged]
  println(intLogged.show)

  val logLogged: Writer[Vector[String], Unit] = Vector("log message 1", "log message 2").tell
  println("Value: " + logLogged.value)
  println("Log: " + logLogged.written)
  println()

  val valLogged: Logged[Int] = 123.writer(Vector("log message 1", "log message 2"))
  println(valLogged.show)

  val writer1 = for {
    a <- 123.pure[Logged]
    _ <- Vector("log message 1", "log message 2").tell
    b <- 345.writer(Vector("log message 3", "log message 4"))
  } yield a + b
  println(writer1.show)

  val writer2 = writer1.mapWritten(_.map(_.toUpperCase))
  println(writer2.show)

  val writer3 = writer2.bimap(
    log => log.map(_.toLowerCase),
    value => value * 2
  )
  println(writer3.show)

  val writer4 = writer3.mapBoth({(log, value) =>
    (log.map(_.toUpperCase + "!"), value / 2)
  })
  println(writer4.show)

  val writer5 = writer4.reset
  println(writer5.show)

  val writer6 = writer5.swap
  println("Value: " + writer6.value)
  println("Log: " + writer6.written)
  println()

}
