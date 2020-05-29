package examples.codec

import CodecInstances._

object CodecTest extends App {
  val i = 123
  val s = "Scala with Cats!"
  val b = true

  val intBox = Box(i)
  val strBox = Box(s)
  val boolBox = Box(b)

  println(s"Encode Int '$i': " + Codec[Int].encode(i))
  println(s"Decode & Encode Int '$i': " + (i == Codec[Int].decode(Codec[Int].encode(i))))
  println()

  println(s"Encode String '$s': " + Codec[String].encode(s))
  println(s"Decode & Encode String '$s': " + (s == Codec[String].decode(Codec[String].encode(s))))
  println()

  println(s"Encode Boolean '$b': " + Codec[Boolean].encode(b))
  println(s"Decode & Encode Boolean '$b': " + (b == Codec[Boolean].decode(Codec[Boolean].encode(b))))
  println()

  println(s"Encode Box[Int] '$intBox': " + Codec[Box[Int]].encode(intBox))
  println(s"Decode & Encode Box[Int] '$intBox': " +
    (intBox == Codec[Box[Int]].decode(Codec[Box[Int]].encode(intBox))))
  println()

  println(s"Encode Box[String] '$strBox': " + Codec[Box[String]].encode(strBox))
  println(s"Decode & Encode Box[String] '$strBox': " +
    (strBox == Codec[Box[String]].decode(Codec[Box[String]].encode(strBox))))
  println()

  println(s"Encode Box[Boolean] '$boolBox': " + Codec[Box[Boolean]].encode(boolBox))
  println(s"Decode & Encode Box[Boolean] '$boolBox': " +
    (boolBox == Codec[Box[Boolean]].decode(Codec[Box[Boolean]].encode(boolBox))))
  println()
}
