package traverse

import cats.Traverse
import cats.instances.option._
import cats.syntax.traverse._

import scala.collection.LinearSeq
import scala.collection.immutable.{HashSet, Queue}

object TraverseInstancesTest extends App {
  import MyTraverseInstances._

  def checkTraverseSyntax[F[_] <: Traversable[_], A](fa: F[A]): Unit = {
    println(fa.traverse(a => Option(a.toString)))
  }

  def checkTraverseImplicits[F[_] <: Traversable[_], A](fa: F[A])(implicit TF: Traverse[F]): Unit = {
    println(TF.traverse(fa)(a => Option(a.toString)))
  }

  checkTraverseSyntax[List, Int](List(1, 2, 3))
  checkTraverseSyntax[Vector, Int](Vector(1, 2, 3))
  checkTraverseSyntax[Seq, Int](Seq(1, 2, 3))
  checkTraverseSyntax[Set, Int](Set(1, 2, 3))
  checkTraverseSyntax[Queue, Int](Queue(1, 2, 3))
  checkTraverseSyntax[LinearSeq, Int](LinearSeq(1, 2, 3))
  checkTraverseSyntax[HashSet, Int](HashSet(1, 2, 3))
  checkTraverseSyntax[Iterable, Int](Iterable(1, 2, 3))
  checkTraverseSyntax[Traversable, Int](Traversable(1, 2, 3))

  println()

  checkTraverseImplicits[List, Int](List(1, 2, 3))
  checkTraverseImplicits[Vector, Int](Vector(1, 2, 3))
  checkTraverseImplicits[Seq, Int](Seq(1, 2, 3))
  checkTraverseImplicits[Set, Int](Set(1, 2, 3))
  checkTraverseImplicits[Queue, Int](Queue(1, 2, 3))
  checkTraverseImplicits[LinearSeq, Int](LinearSeq(1, 2, 3))
  checkTraverseImplicits[HashSet, Int](HashSet(1, 2, 3))
  checkTraverseImplicits[Iterable, Int](Iterable(1, 2, 3))
  checkTraverseImplicits[Traversable, Int](Traversable(1, 2, 3))
}
