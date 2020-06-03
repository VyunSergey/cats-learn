package monaderror

object MyMonadErrorInstances {
  type ErrorOr[A] = Either[String, A]

  implicit val errorOrMonadError: MyMonadError[ErrorOr, String] = new MyMonadError[ErrorOr, String] {
    def raiseError[A](e: String): ErrorOr[A] = Left(e)
    def handleError[A](fa: ErrorOr[A])(f: String => A): ErrorOr[A] = Right(fa.fold(f, identity[A]))
    def ensure[A](fa: ErrorOr[A])(e: String)(f: A => Boolean): ErrorOr[A] = fa.filterOrElse(f, e)
    def pure[A](value: A): ErrorOr[A] = Right(value)
    def flatMap[A, B](fa: ErrorOr[A])(f: A => ErrorOr[B]): ErrorOr[B] = fa.flatMap(f)
  }
}
