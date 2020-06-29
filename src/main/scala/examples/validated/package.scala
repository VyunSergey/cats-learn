package examples

import cats.data.Validated

package object validated {
  type Error = List[String]
  type FormData = Map[String, String]
  type FailFast[A] = Either[Error, A]
  type FailSlow[A] = Validated[Error, A]
}
