package functor

import cats.Functor
import cats.instances.all._
import cats.syntax.functor._
import FunctorLaws._

import scala.util.Try

object FunctorTest extends App {
  val i = 1
  val li = List(i)
  val oi = Option(i)
  val ti = Try(i)

  val f = (_: Int) + 1
  val g = (_: Int) * 2

  val s = "Hello"
  val ls = List(s)
  val os = Option(s)
  val ts = Try(s)

  val fs = (_: String) + " with"
  val gs = (_: String) + " Cats!"

  val listF: Functor[List] = Functor[List]
  val opF: Functor[Option] = Functor[Option]
  val tryF: Functor[Try] = Functor[Try]

  println(s"$li.map(_ + 1).map(_ * 2): " + li.map(f).map(g))
  println(s"F.map(F.map($li)(_ + 1))(_ * 2): " + listF.map(listF.map(li)(f))(g))
  println(s"F.lift(g compose f)($li): " + listF.lift(g compose f)(li))
  println()

  println("check Identity Law for List with Int: " + identityLaw(li))
  println("check Composition Law for List with Int: " + compositionLaw(li)(f)(g))
  println()

  println(s"$oi.map(_ + 1).map(_ * 2): " + oi.map(f).map(g))
  println(s"F.map(F.map($oi)(_ + 1))(_ * 2): " + opF.map(opF.map(oi)(f))(g))
  println(s"F.lift(g compose f)($oi): " + opF.lift(g compose f)(oi))
  println()

  println("check Identity Law for Option with Int: " + identityLaw(oi))
  println("check Composition Law for Option with Int: " + compositionLaw(oi)(f)(g))
  println()

  println(s"$ti.map(_ + 1).map(_ * 2): " + ti.map(f).map(g))
  println(s"F.map(F.map($ti)(_ + 1))(_ * 2): " + tryF.map(tryF.map(ti)(f))(g))
  println(s"F.lift(g compose f)($ti): " + tryF.lift(g compose f)(ti))
  println()

  println("check Identity Law for Try with Int: " + identityLaw(ti))
  println("check Composition Law for Try with Int: " + compositionLaw(ti)(f)(g))
  println()

  println(s"$ls.map(_ + 1).map(_ * 2): " + ls.map(fs).map(gs))
  println(s"F.map(F.map($ls)(_ + 1))(_ * 2): " + listF.map(listF.map(ls)(fs))(gs))
  println(s"F.lift(g compose f)($ls): " + listF.lift(gs compose fs)(ls))
  println()

  println("check Identity Law for List with String: " + identityLaw(ls))
  println("check Composition Law for List with String: " + compositionLaw(ls)(fs)(gs))
  println()

  println(s"$os.map(_ + 1).map(_ * 2): " + os.map(fs).map(gs))
  println(s"F.map(F.map($os)(_ + 1))(_ * 2): " + opF.map(opF.map(os)(fs))(gs))
  println(s"F.lift(g compose f)($os): " + opF.lift(gs compose fs)(os))
  println()

  println("check Identity Law for Option with String: " + identityLaw(os))
  println("check Composition Law for Option with String: " + compositionLaw(os)(fs)(gs))
  println()

  println(s"$ts.map(_ + 1).map(_ * 2): " + ts.map(fs).map(gs))
  println(s"F.map(F.map($ts)(_ + 1))(_ * 2): " + tryF.map(tryF.map(ts)(fs))(gs))
  println(s"F.lift(g compose f)($ts): " + tryF.lift(gs compose fs)(ts))
  println()

  println("check Identity Law for Try with String: " + identityLaw(ts))
  println("check Composition Law for Try with String: " + compositionLaw(ts)(fs)(gs))
  println()
}
