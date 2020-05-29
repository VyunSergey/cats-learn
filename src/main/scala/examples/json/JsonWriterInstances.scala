package examples.json

object JsonWriterInstances {
  implicit val stringJsonWriter: JsonWriter[String] =
    (value: String) => JsString(value)

  implicit val numericJsonWriter: JsonWriter[Double] =
    (value: Double) => JsNumber(value)

  implicit val personJsonWriter: JsonWriter[Person] =
    (person: Person) => JsObject(Map(
      "name" -> JsString(person.name),
      "email" -> JsString(person.email)
    ))

  implicit def optionWriter[A](implicit w: JsonWriter[A]): JsonWriter[Option[A]] =
    {
      case Some(v) => w.write(v)
      case None => JsNull
    }
}
