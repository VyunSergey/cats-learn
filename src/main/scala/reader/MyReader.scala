package reader

final case class MyReader[A, B](run: A => B) {
  def dimap[C, D](f: C => A)(g: B => D): MyReader[C, D] = MyReader(c => g(run(f(c))))
  def map[C](f: B => C): MyReader[A, C] = dimap(identity[A])(f)
  def local[AA](f: AA => A): MyReader[AA, B] = dimap(f)(identity[B])
  def andThen[C](f: B => C): MyReader[A, C] = map(f)
  def compose[AA](f: AA => A): MyReader[AA, B] = local(f)
  def apply(a: A): B = run(a)
}
