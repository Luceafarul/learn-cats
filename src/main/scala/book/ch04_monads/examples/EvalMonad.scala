package book.ch04_monads.examples

object EvalMonad extends App {
  // Eager with memoized - evaluating when defined (call-by-value)
  val x: Double = {
    println("Computing x...")
    math.random
  }
  println(x)
  println(x)

  // Lazy and non memoized - evaluating each time when called (call-by-name)
  def y: Double = {
    println("Computing y...")
    math.random
  }

  println(y)
  println(y)

  // Lazy and memoized - evaluating only when called and memoized result (call-by-need)
  lazy val c: Double = {
    println("Computing c...")
    math.random
  }
  println(c)
  println(c)



  // Eval

  import cats.Eval

  val now = Eval.now(math.random + 1000)
  println(now)

  val always = Eval.now(math.random + 3000)
  println(always)

  val later = Eval.now(math.random + 5000)
  println(later)

  val evalX = Eval.now {
    println("Computing x:")
    math.random
  }
  println(evalX.value)
  println(evalX.value)

  val evalY = Eval.always {
    println("Computing y:")
    math.random
  }
  println(evalY.value)
  println(evalY.value)

  val evalC = Eval.later {
    println("Computing c:")
    math.random
  }
  println(evalC.value)
  println(evalC.value)

  val greeting = Eval.always {
    println("Step 1")
    "Hello"
  }.map { s =>
    println("Step 2")
    s"$s world!"
  }
  println(greeting.value)

  val answer =
    for {
      a <- Eval.now {
        println("Calculating A")
        37
      }
      b <- Eval.always {
        println("Calculating B")
        36
      }
    } yield {
      println("Adding A and B:")
      a + b
    }

  println(answer.value)
  println(answer.value)

  val saying = Eval
    .always {
      println("Step 1")
      "The cat"
    }
    .map { s =>
      println("Step 2")
      s"$s sat on"
    }
    .memoize
    .map { s =>
      println("Step 3")
      s"$s the mat"
    }

  println(saying.value)
  println(saying.value)

  def factorial(n: BigInt): Eval[BigInt] =
    if (n == 1) Eval.now(n) else Eval.defer(factorial(n - 1).map(_ * n))

  println(factorial(500).value)

  def foldRight[A, B](xs: List[A], acc: B)(f: (A, B) => B): B =
    xs match {
      case head :: tail => f(head, foldRight(tail, acc)(f))
      case Nil => acc
    }

  def foldRightEval[A, B](xs: List[A], acc: B)(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    xs match {
      case head :: tail => Eval.defer(f(head, foldRightEval(tail, acc)(f)))
      case Nil => Eval.now(acc)
    }
}
