package state

import cats.Monoid

final case class MyState[SA, SB, A](run: SA => (SB, A)) {
  def flatMap[SC, B](fas: A => MyState[SB, SC, B]): MyState[SA, SC, B] =
    MyState(run andThen {case (s, a) => fas(a).run(s)})

  def map[B](f: A => B): MyState[SA, SB, B] =
    transform((s, a) => (s, f(a)))

  def bimap[SC, B](f: SB => SC, g: A => B): MyState[SA, SC, B] =
    transform((s, a) => (f(s), g(a)))

  def dimap[S0, S1](f: S0 => SA)(g: SB => S1): MyState[S0, S1, A] =
    contramap(f).modify(g)

  def contramap[S0](f: S0 => SA): MyState[S0, SB, A] =
    MyState(run compose f)

  def transform[SC, B](f: (SB, A) => (SC, B)): MyState[SA, SC, B] =
    MyState(run andThen {case (s, a) => f(s, a)})

  def modify[SC](f: SB => SC): MyState[SA, SC, A] =
    transform((s, a) => (f(s), a))

  def runS(initial: SA): SB = run(initial)._1

  def runA(initial: SA): A = run(initial)._2

  def runEmpty(implicit S: Monoid[SA]): (SB, A) = run(S.empty)

  def runEmptyS(implicit S: Monoid[SA]): SB = runEmpty._1

  def runEmptyA(implicit S: Monoid[SA]): A = runEmpty._2
}
