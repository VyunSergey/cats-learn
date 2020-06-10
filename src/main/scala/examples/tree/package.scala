package examples

import cats.Show

package object tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)

  def treeShown[A]: Show[Tree[A]] = new Show[Tree[A]] {
    def show(tree: Tree[A]): String = tree match {
      case Branch(left, right) =>
        val (lShow, rShow) = (show(left), show(right))

        s""" / \\
           |$lShow $rShow
           |""".stripMargin
      case Leaf(value) => value.toString
    }
  }

  def nSp(n: Int): String = " " * n
}
