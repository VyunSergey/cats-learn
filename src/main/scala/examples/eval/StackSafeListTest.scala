package examples.eval

import examples.eval.StackSafeListSyntax._

object StackSafeListTest extends App {
  val lst1: List[BigInt] = (1 to 1000).toList.map(BigInt(_))
  println(lst1.take(10) ::: lst1.takeRight(10))

  val one: BigInt = BigInt(1)
  val fn1 = (a: BigInt, b: BigInt) => (1 + a + b).gcd(b)

  println(lst1.foldLeft(one)(fn1))
  println(lst1.safeFoldLeft(one)(fn1))
  println(lst1.foldRight(one)(fn1))
  println(lst1.safeFoldRight(one)(fn1))

  val lst2: List[BigInt] = (1 to 10000).toList.map(BigInt(_))
  println(lst2.take(10) ::: lst2.takeRight(10))

  val emptySeq: Seq[BigInt] = Seq.empty[BigInt]
  val fn2L = (b: Seq[BigInt], a: BigInt) => a +: b
  val fn2R = (a: BigInt, b: Seq[BigInt]) => a +: b

  println(lst2.foldLeft(emptySeq)(fn2L).take(100))
  println(lst2.safeFoldLeft(emptySeq)(fn2L).take(100))
  println(lst2.foldRight(emptySeq)(fn2R).take(100))
  println(lst2.safeFoldRight(emptySeq)(fn2R).take(100))

}
