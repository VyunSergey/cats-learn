package reader

object MyReaderInstances {
  implicit def stringReader[A]: MyReader[A, String] = MyReader(_.toString)
  implicit def listReader[A]: MyReader[A, List[A]] = MyReader(a => List(a))
  implicit def vectReader[A]: MyReader[A, Vector[A]] = MyReader(a => Vector(a))
}
