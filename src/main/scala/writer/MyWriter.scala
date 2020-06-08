package writer

trait MyWriter[F[_], W, A] {
  def log(value: W): W
  def result(value: A): F[A]
}
