package examples.tree

import cats.Monad

import scala.annotation.tailrec

class TreeMonad extends Monad[Tree] {
  def pure[A](x: A): Tree[A] = leaf(x)

  def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
    case Branch(left, right) => branch(flatMap(left)(f), flatMap(right)(f))
    case Leaf(value) => f(value)
  }

  def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
    @tailrec
    def loop(open: List[Tree[Either[A, B]]], closed: List[Option[Tree[B]]]): List[Tree[B]] =
      open match {
        case Branch(left, right) :: tail => loop(left :: right :: tail, None :: closed)
        case Leaf(Left(la)) :: tail => loop(f(la) :: tail, closed)
        case Leaf(Right(ra)) :: tail => loop(tail, Some(pure(ra)) :: closed)
        case Nil =>
          closed.foldLeft(List.empty[Tree[B]])({(acc: List[Tree[B]], maybeTree: Option[Tree[B]]) =>
            maybeTree.map(_ :: acc).getOrElse({
              val left :: right :: tail = acc
              branch(left, right) :: tail
            })
          })
      }

    loop(List(f(a)), Nil).head
  }
}

object TreeMonad extends TreeMonad
