package examples.json

import JsonWriterInstances._
import JsonSyntax._

object JsonTest extends App {
  val p1: Person = Person("Vlad", "vald@gmail.com")
  val p2: Person = Person("Serge", "serge@icloud.com")
  val ops1: Option[String] = Some("123")
  val ops2: Option[String] = None

  val jp1: Json = Json.toJson(p1)
  val jp2: Json = p2.toJson
  val j3: Json = implicitly[JsonWriter[String]].write("STRING")
  val j4: Json = ops1.toJson
  val j5: Json = ops2.toJson

  println(jp1)
  println(jp2)
  println(j3)
  println(j4)
  println(j5)

}
