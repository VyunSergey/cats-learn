package examples.codec

object CodecInstances {
  implicit val stringCodec: Codec[String] = new Codec[String] {
    def encode(value: String): String = value
    def decode(value: String): String = value
  }

  implicit val intCodec: Codec[Int] = stringCodec.imap(_.toInt, _.toString)
  implicit val booleanCodec: Codec[Boolean] = stringCodec.imap(_.toBoolean, _.toString)

  implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] = new Codec[Box[A]] {
    def encode(value: Box[A]): String = c.encode(value.value)
    def decode(value: String): Box[A] = Box(c.decode(value))
  }

}
