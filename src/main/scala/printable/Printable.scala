package printable

trait Printable[A] {
  def format(value: A): String
  def contramap[B](f: B => A): Printable[B] = (value: B) => format(f(value))
}

object Printable {
  def apply[A](implicit p: Printable[A]): Printable[A] = p

  def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)

  def print[A](value: A)(implicit p: Printable[A]): Unit = println(format(value))
}
