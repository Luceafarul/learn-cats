package courses.rockthejvm.part3

object _4_State extends App {

  type WhatIsState[S, A] = S => (S, A)

  import cats.data.State

  val countAndSay: State[Int, String] = State(currentCount => (currentCount + 1, s"Counted ${currentCount + 1}"))
  val (eleven, string) = countAndSay.run(10).value

  println(s"Result: $eleven and $string")

  // State: "Iterative" computations

  // Iterative
  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, obtained $a"
  a *= 5
  val secondComputation = s"Multiplied with 5, obtained $a"

  // Pure FP with States
  val firstTransformation = State((n: Int) => (n + 1, s"Added 1 to 10, obtained ${n + 1}"))
  val secondTransformation = State((n: Int) => (n * 5, s"Multiplied with 5, obtained ${n * 1}"))
  val compositeTransformation: State[Int, (String, String)] = firstTransformation.flatMap { firstResult =>
    secondTransformation.map(secondResult => (firstResult, secondResult))
  }
  val compositeForTransformation =
    for {
      firstResult <- firstTransformation
      secondResult <- secondTransformation
    } yield (firstResult, secondResult)

  println(compositeTransformation.run(10).value)
  println(compositeForTransformation.run(10).value)

  val func1 = (n: Int) => (n + 1, s"Added 1 to 10, obtained ${n + 1}")
  val func2 = (n: Int) => (n * 5, s"Multiplied with 5, obtained ${n * 1}")
  val compositeFuncs = func1.andThen {
    case (newState, firstResult) =>
      val (finalState, secondResult) = func2(newState)
      (finalState, (firstResult, secondResult))
  }

  println(compositeFuncs(10))

  // Exercise:
  // online store
  final case class ShoppingCart(items: List[String], total: Double)

  def addToCart(item: String, price: Double): State[ShoppingCart, Double] = State { sc =>
    val shoppingCart = ShoppingCart(sc.items :+ item, price + sc.total)
    (shoppingCart, shoppingCart.total)
  }

  println(addToCart("iPhone", 899.99).run(ShoppingCart(List("iPad"), 799.99)).value)

  val danielsCart =
    for {
      _ <- addToCart("Fender guitar", 499.99)
      _ <- addToCart("Elixir strings", 19.99)
      total <- addToCart("Electric cable", 7.99)
    } yield total

  println(danielsCart.run(ShoppingCart(List.empty, 0)).value)

  // Exercise: Pure mental gymnastics
  // Returns a State data structure that, when run, will not change the state but will issue the value f(a)
  def inspect[A, B](f: A => B): State[A, B] =
    State(a => (a, f(a)))

  // Returns a State data structure that, when run, returns the value of that state and makes no changes
  def get[A]: State[A, A] =
    State(a => (a, a))

  // Returns a State data structure that, when run, returns Unit and sets the state to that value
  def set[A](a: A): State[A, Unit] =
    State(_ => (a, ()))

  // Returns a State data structure that, when run, will return Unit and sets the state to f(state)
  def modify[A](f: A => A): State[A, Unit] =
    State(a => (f(a), ()))

  val program: State[Int, (Int, Int, Int)] =
    for {
      a <- get[Int]
      _ <- set[Int](a + 10)
      b <- get[Int]
      _ <- modify[Int](_ + 37)
      c <- inspect[Int, Int](_ * 3)
    } yield (a, b, c)

  println(program.run(1).value)
}
