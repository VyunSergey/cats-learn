package examples.codec

import CodecInstances._

object CodecTest extends App {
  val i = 123
  val s = "Scala with Cats!"
  val b = true

  println(s"Decode & Encode Int '$i': " + (i == Codec[Int].decode(Codec[Int].encode(i))))
  println(s"Decode & Encode String '$s': " + (s == Codec[String].decode(Codec[String].encode(s))))
  println(s"Decode & Encode Boolean '$b': " + (b == Codec[Boolean].decode(Codec[Boolean].encode(b))))

}
