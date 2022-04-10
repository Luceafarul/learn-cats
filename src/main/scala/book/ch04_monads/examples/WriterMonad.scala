package book.ch04_monads.examples

import cats.data.Writer
import cats.instances.vector._
import cats.syntax.writer._
import cats.syntax.applicative._

object WriterMonad extends App {
  val w = Writer(Vector("It was the best of times", "It was the worst of times"), 1859)
  println(w)

  type Logged[A] = Writer[Vector[String], A]

  val oneTwoThree = 123.pure[Logged]
  println(oneTwoThree)

  val resultOne = Vector("Unit").tell
  println(resultOne)

  val resultTwo = Writer(Vector("One", "Two", "Three"), 123)
  println(resultTwo)

  val resultThree = 123.writer(Vector("One", "Two", "Three"))
  println(resultThree)

  // Extracting values
  val value = resultThree.value
  println(value)

  val log = resultThree.written
  println(log)

  val (v, l) = resultThree.run
  println(s"Value: $v and log: $l")

  // Composing and Transforming
  val writer1 =
    for {
      a <- 10.pure[Logged]
      _ <- Vector("a", "b", "c").tell
      b <- 27.writer(Vector("e", "f", "g"))
    } yield a + b
  println(s"Writer 1: ${writer1.run}")

  val writer2 = writer1.mapWritten(_.map(_.toUpperCase))
  println(s"Writer 2: ${writer2.run}")

  val writer3 = writer1.bimap(
    log => log.map(_.toUpperCase),
    v => v * 100
  )
  println(s"Writer 3: ${writer3.run}")

  val writer4 = writer1.mapBoth((log, v) => (log.map(_ + "!"), v * 1000))
  println(s"Writer 4: ${writer4.run}")

  val writer5 = writer1.swap
  println(s"Writer 5: ${writer5.run}")

  val writer6 = writer1.reset
  println(s"Writer 6: ${writer6.run}")

  // Exercise:
  def slowly[A](body: => A) = try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  def factorialWriter(n: Int): Logged[Int] = {
    if (n == 0) Writer(Vector(s"fact $n 1"), 1)
    else {
      factorialWriter(n - 1)
        .map(_ * n)
        .mapBoth((l, v) => (l ++ Vector(s"fact $n $v"), v))
    }
  }

  def solutionFactorial(n: Int): Logged[Int] =
    for {
      ans <- if (n == 0) {
        1.pure[Logged]
      } else {
        slowly(solutionFactorial(n - 1).map(_ * n))
      }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans

  factorial(5)

  // Parallel run

  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits._

  val result = Await.result(Future.sequence(Vector(Future(factorial(5)), Future(factorial(5)))), 5.seconds)
  println(result)

  val anotherResult = Await.result(Future.sequence(Vector(Future(factorialWriter(5)), Future(factorialWriter(5)))), 5.seconds)
  println(anotherResult)
}
