package examples.tree

import cats.Show
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.show._

object TreeMonadTest extends App {
  val leaf1: Tree[Int] = leaf(1)
  val leaf2: Tree[Int] = leaf(2)
  val branch1: Tree[Int] = branch(leaf1, leaf2)
  val leaf3: Tree[Int] = leaf(3)
  val branch2: Tree[Int] = branch(leaf3, branch1)
  val tree: Tree[Int] = branch(leaf(0), branch2)

  implicit val treeShow: Show[Tree[Int]] = treeShown[Int]

  println(tree.show)

  implicit val treeMonad: TreeMonad = TreeMonad
  val tree2 = for {
    l1 <- 1.pure[Tree]
    l2 <- 2.pure[Tree]
    b1 = branch(leaf(l1), leaf(l2))
    l3 <- 3.pure[Tree]
    b2 = branch(leaf(l3), b1)
    tr <- branch(leaf(0), b2)
  } yield tr

  println(tree2.show)

  val tree3 = for {
    a <- 1.pure[Tree]
    b <- branch(leaf(a - 1), leaf(a + 1))
    c <- branch(leaf(b * 2), leaf(b / 2))
  } yield c

  println(tree3.show)

}
