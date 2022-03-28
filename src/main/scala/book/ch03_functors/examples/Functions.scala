package book.ch03_functors.examples

object Functions extends App {
  val functionOne: Int => Double = x => x.toDouble
  val functionTwo: Double => Double = x => x * 2

  import cats.syntax.functor._

  val res1 = (functionOne map functionTwo)(2)
  val res2 = (functionOne andThen functionTwo)(2)
  val res3 = functionTwo(functionOne(2))

  println(res1)
  println(res2)
  println(res3)

  val functionThree = ((x: Int) => x.toDouble)
    .map(x => x + 1)
    .map(x => x * 2)
    .map(x => s"$x!")

  println(functionThree(123))
}
