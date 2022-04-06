package courses.rockthejvm.part3

object _3_Evaluation extends App {

  // Cats makes the distinction between:
  // - evaluating an expression eagerly
  // - evaluating lazily and every time when request it
  // - evaluating lazily and keeping the value (memoizing)

  import cats.Eval

  // Evaluating when defined (like val)
  val instantEval: Eval[Int] = Eval.now {
    println("Computing now!")
    73
  }

  println(instantEval.value)
  println(instantEval.value)

  // Evaluating every time when requested (like def)
  val redoEval = Eval.always {
    println("Computing always!")
    37
  }

  println(redoEval.value)
  println(redoEval.value)

  // Evaluating once when requested and memoized value (like lazy val + memoized(?))
  val delayEval = Eval.later {
    println("Computing later!")
    13
  }

  println(delayEval.value)
  println(delayEval.value)

  val composedEval = instantEval.flatMap(v1 => delayEval.map(v2 => v1 + v2))

  println(composedEval.value)
  println(composedEval.value)

  val anotherComposedEval =
    for {
      v1 <- instantEval
      v2 <- delayEval
    } yield v1 + v2

  println(anotherComposedEval.value)
  println(anotherComposedEval.value)

  val exerciseEval =
    for {
      a <- delayEval
      b <- redoEval
      c <- instantEval
      d <- redoEval
    } yield a + b + c + d

  println(exerciseEval.value)
  println(exerciseEval.value)

  // Remember a compute value
  val doNotRecompute = redoEval.memoize

  println(doNotRecompute.value)
  println(doNotRecompute.value)

  val tutorial = Eval
    .always {
      println("Step 1 ...")
      "Put the guitar on you leg"
    }
    .map { step1 =>
      println("Step 2 ...")
      s"$step1 then put your left hand on the neck"
    }
    .memoize // Remember the value up to this point
    .map { step12 => println("Step 3, more complicated"); s"$step12 then with the right hand strike the strings" }

  println(tutorial.value)
  println(tutorial.value)

  // Exercise:
  // Implement defer such that defer(Eval.now) does NOT run the side effects
  def defer[T](eval: => Eval[T]): Eval[T] = Eval.defer(eval)
  def solutionDefer[T](eval: => Eval[T]): Eval[T] =
    Eval.later(()).flatMap(_ => eval)

  defer(Eval.now {
    println("Now!")
    47
  })
  solutionDefer(Eval.now {
    println("Now!")
    47
  })

  // Exercise:
  // Rewrite the method with Eval
  def reverse[T](xs: List[T]): List[T] =
    if (xs.isEmpty) xs
    else reverse(xs.tail) :+ xs.head

  println(reverse(List(1, 2, 3, 4, 5)))

  def reverseEval[T](xs: List[T]): Eval[List[T]] =
    if (xs.isEmpty) Eval.later(xs)
    else solutionDefer(reverseEval(xs.tail).map(_ :+ xs.head))

  println(reverseEval(List(1, 2, 3, 4, 5)).value)
  println(reverseEval((1 to 10000).toList).value)
}
