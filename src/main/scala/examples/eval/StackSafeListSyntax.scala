package examples.eval

import cats.Eval

object StackSafeListSyntax {
  implicit class StackSafeListOps[A](lst: List[A]) {
    def safeFoldLeft[B](acc: B)(f: (B, A) => B): B = {
      def innerFoldLeft(as: List[A], acc: B)(f: (B, A) => B): Eval[B] = as match {
        case body :+ element => Eval.defer(innerFoldLeft(body, acc)(f)).map(f(_, element))
        case Nil => Eval.now(acc)
      }
      innerFoldLeft(lst, acc)(f).value
    }

    def safeFoldRight[B](acc: B)(f: (A, B) => B): B = {
      def innerFoldRight(as: List[A], acc: B)(f: (A, B) => B): Eval[B] = as match {
        case head :: tail => Eval.defer(innerFoldRight(tail, acc)(f)).map(f(head, _))
        case Nil => Eval.now(acc)
      }
      innerFoldRight(lst, acc)(f).value
    }
  }
}
