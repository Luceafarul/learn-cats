package book.ch04_monads

import cats.data.Writer
import cats.syntax.writer._
import cats.instances.vector._
import cats.syntax.applicative._

object LoggingInMultithread extends App {
  def slowly[A](body: => A) =
    try body
    finally Thread.sleep(110)

  def factorial(n: Int): Int = {
    val answer = slowly(if (n == 1) n else n * factorial(n - 1))
    println(s"[${Thread.currentThread.getName}] Factorial $n = $answer")
    answer
  }

  def factorialWriter(n: Int): Writer[Vector[String], Int] = {
    val answer = slowly {
      if (n == 1)
        n.writer(Vector(s"[${Thread.currentThread.getName}] Factorial $n = $n"))
      else {
        val (log, res) = factorialWriter(n - 1).run
        val answer = n * res
        Writer(
          log :+ s"[${Thread.currentThread.getName}] Factorial $n = $answer",
          answer
        )
      }
    }
    answer
  }

  val (log, res) = factorialWriter(5).run
  log.foreach(println)
  println(res)

  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  Await.result(
    Future.sequence(
      Vector(
        Future(factorial(5)),
        Future(factorial(5))
      )
    ),
    5.seconds
  )

  val writerRes = Await.result(
    Future.sequence(
      Vector(
        Future(factorialWriter(5)),
        Future(factorialWriter(5))
      )
    ),
    5.seconds
  )

  writerRes.foreach { writer =>
    println()
    val (log, res) = writer.run
    log.foreach(println)
    println(s"Result: $res")
  }
}
