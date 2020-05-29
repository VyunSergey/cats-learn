package examples.json

trait JsonWriter[A] {
  def write(value: A): Json
}

object JsonWriter {
  def apply[A](implicit w: JsonWriter[A]): JsonWriter[A] = w
}
