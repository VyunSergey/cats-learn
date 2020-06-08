package writer

object MyWriterInstances {
  implicit def stringLogOptionFWriters[A]: MyWriter[Option, String, A] = new MyWriter[Option, String, A] {
    def log(value: String): String = value
    def result(value: A): Option[A] = Option(value)
  }

  implicit def stringLogListFWriters[A]: MyWriter[List, String, A] = new MyWriter[List, String, A] {
    def log(value: String): String = value
    def result(value: A): List[A] = List(value)
  }
}
