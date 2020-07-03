package traverse

import cats.{Applicative, Eval, Traverse}

object MyTraverseInstances {
  /**
   * Instances for Traverse of any subtype of scala.collection.Traversable[A]
   */
  implicit def subTraversableTraverseInst[F[_] <: Traversable[_]]: Traverse[F] = new Traverse[F] {
    def traverse[G[_], A, B](fa: F[A])(f: A => G[B])
                            (implicit G: Applicative[G]): G[F[B]] = {
      val streamF: Stream[A] = fa.asInstanceOf[Traversable[A]].toStream

      def loop(i: Int): Eval[G[Traversable[B]]] =
        if (i < fa.size) G.map2Eval(f(streamF(i)), Eval.defer(loop(i + 1)))(Traversable(_) ++ _)
        else Eval.now(G.pure(Traversable.empty[B]))

      G.map(loop(0).value)(_.asInstanceOf[F[B]])
    }

    def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
      fa.asInstanceOf[Traversable[A]].foldLeft(b)(f)

    def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.asInstanceOf[Traversable[A]].foldRight(lb)(f)
  }
}
