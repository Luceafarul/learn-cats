package book.ch03_functors.examples

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.Random

object ReferentialTransparency extends App {
  val futureOne = {
    // Initialize Random with a fixed seed:
    val r = new Random(0L)

    // nextInt has the side-effect of moving to
    // the next random number in the sequence:
    val x = Future(r.nextInt)

    for {
      a <- x
      b <- x
    } yield (a, b)
  }

  val futureTwo = {
    val r = new Random(0L)

    for {
      a <- Future(r.nextInt)
      b <- Future(r.nextInt)
    } yield (a, b)
  }

  val resultOne = Await.result(futureOne, 1.second)
  val resultTwo = Await.result(futureTwo, 1.second)

  println(s"Future result one: $resultOne")
  println(s"Future result two: $resultTwo")
}
