package courses.rockthejvm.part5

object _1_Kleisli extends App {
  // Kleisli enables composition of functions that return a monadic value.
  // Represents a function `A => F[B]`.
  // Used for:
  // - function composition when they return F[_] types
  // - dependency injection - Reader

  val func1: Int => Option[String] = x => if (x % 2 == 0) Some(s"$x is even") else None
  val func2: Int => Option[Int] = x => Some(x * 3)

  // Does not compile:
  // val func3 = func2 andThen func1
  // because type mismatch:
  // found   : Int => Option[String]
  // required: Option[Int] => ?

  val plainFunc1: Int => String = x => if (x % 2 == 0) s"$x is even" else s"$x is not even"
  val plainFunc2: Int => Int = x => x * 3
  val plainFunc3: Int => String = plainFunc2.andThen(plainFunc1)
  println(s"Compose plain function: ${plainFunc3(3)}")
  println(s"Compose plain function: ${plainFunc3(4)}")

  import cats.data.Kleisli

  // Wrap a function from Int to Option[String]
  val func1K: Kleisli[Option, Int, String] = Kleisli(func1)
  val func2K: Kleisli[Option, Int, Int] = Kleisli(func2)
  val func3K: Kleisli[Option, Int, String] = func2K.andThen(func1K)
  println(s"Compose Kleisli function: ${func3K(3)}")
  println(s"Compose Kleisli function: ${func3K(4)}")

  // Convenience:
  val multiply = func2K.map(_ * 2) // Option(...).map(_ * 2)
  val chain = func2K.flatMap(_ => func1K)

  // Exercise:
  import cats.Id
  type InterestingKleisli[A, B] = Kleisli[Id, A, B] // Wrapper over A => Id[B]

  // Hint:
  val times2 = Kleisli[Id, Int, Int](x => x * 2)
  val plus4 = Kleisli[Id, Int, Int](x => x + 4)
  val compose = times2.flatMap(x => plus4.map(y => x + y))

  println(compose.run(5)) // (5 * 2) + (5 + 4) = 19

  val composeFor =
    for {
      t2 <- times2
      p4 <- plus4
    } yield t2 + p4

  // InterestingKleisli == Reader
  // Kleisli[Id, Int, Int] == Reader[Int, Int]
}
