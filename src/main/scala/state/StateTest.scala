package state

import cats.data.State

object StateTest extends App {
  val a: State[Int, String] = State[Int, String]({ state =>
    (state, s"The state is $state")
  })
  println(a)

  val (state, res) = a.run(10).value
  println(s"State: $state Result: $res")

  val justState = a.runS(10).value
  val justResult = a.runA(10).value
  println(s"State: $justState Result: $justResult")

  val step1 = State[Int, String]({ num =>
    val ans = num + 1
    (ans, s"Result of the step1: $ans")
  })

  val step2 = State[Int, String]({ num =>
    val ans = 2 * num
    (ans, s"Result of the step2: $ans")
  })

  val both = for {
    a <- step1
    b <- step2
  } yield (a, b)

  val (bState, bRes) = both.run(10).value
  println(s"State: $bState Result: $bRes")

  // `get` extracts the State as the Result
  val get = State.get[Int]
  val (gState, gRes) = get.run(10).value
  println(s"State: $gState Result: $gRes")

  // `set` updates the State and returns Unit as the Result
  val set = State.set[Int](30)
  val (sState, sRes) = set.run(10).value
  println(s"State: $sState Result: $sRes")

  // `pure` ignores the State and returns a supplied Result
  val pure = State.pure[Int, String]("This is the Result!")
  val (pState, pRes) = pure.run(10).value
  println(s"State: $pState Result: $pRes")

  // `inspect` extracts the State using transformation function
  val inspect = State.inspect[Int, String](s => s"New State: $s")
  val (iState, iRes) = inspect.run(10).value
  println(s"State: $iState Result: $iRes")

  // `modify` updates the State using updating function
  val modify = State.modify[Int](s => 2 * s + 1)
  val (mState, mRes) = modify.run(10).value
  println(s"State: $mState Result: $mRes")

  val program: State[Int, (Int, Int, Int)] = for {
    a <- State.get[Int]
    _ <- State.set[Int](a + 1)
    b <- State.get[Int]
    _ <- State.modify[Int](_ + 1)
    c <- State.inspect[Int, Int](_ * 1000)
  } yield (a, b, c)
  val (prmState, prmRes) = program.run(10).value
  println(s"State: $prmState Result: $prmRes")

}
