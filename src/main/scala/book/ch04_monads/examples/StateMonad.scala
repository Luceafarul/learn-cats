package book.ch04_monads.examples

import cats.data.State

object StateMonad extends App {
  val aState = State[Int, String] { state =>
    (state, s"The state is: $state")
  }

  val (state, result) = aState.run(10).value
  println(s"State: '$state' and result: $result")

  val justTheState = aState.runS(10).value
  println(s"Just the state: $justTheState")

  val justTheResult = aState.runA(10).value
  println(s"Just the result: $justTheResult")

  // Composing and Transforming
  val step1 = State[Int, String] { n =>
    val r = n + 1
    (r, s"Result of step 1: $r")
  }

  val step2 = State[Int, String] { n =>
    val r = n * 2
    (r, s"Result of step 2: $r")
  }

  val both =
    for {
      a <- step1
      b <- step2
    } yield (a, b)

  println(s"Result of both steps: ${both.run(20).value}")

  val getDemo = State.get[Int]
  println(s"get demo: ${getDemo.run(10).value}")

  val setDemo = State.set[Int](30)
  println(s"set demo: ${setDemo.run(10).value}")

  val pureDemo = State.pure[Int, String]("Result")
  println(s"pure demo: ${pureDemo.run(10).value}")

  val inspectDemo = State.inspect[Int, String](n => s"Current state: $n")
  println(s"inspect demo: ${inspectDemo.run(10).value}")

  val modifyDemo = State.modify[Int](_ + 11)
  println(s"modify demo: ${modifyDemo.run(10).value}")

  val program: State[Int, (Int, Int, Int)] =
    for {
      a <- State.get[Int]
      _ <- State.set[Int](a * 2)
      b <- State.get[Int]
      _ <- State.modify[Int](_ + 1)
      c <- State.inspect[Int, Int](_ * 1000)
    } yield (a, b, c)

  println(s"Program result: ${program.run(1).value}")
}
