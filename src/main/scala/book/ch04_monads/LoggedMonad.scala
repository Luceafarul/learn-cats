package book.ch04_monads

import cats.data.Writer
import cats.syntax.writer._
import cats.instances.vector._
import cats.syntax.applicative._

object LoggedMonad {
  type Logged[A] = Writer[Vector[String], A]

  def factorial(n: Int): Logged[Int] =
    for {
      answer <- if (n == 0 || n == 1) {
        1.pure[Logged]
      } else {
        LoggingInMultithread.slowly(factorial(n - 1).map(_ * n))
      }
      _ <- Vector(s"[${Thread.currentThread.getName}] Factorial $n = $answer").tell
    } yield answer
}
