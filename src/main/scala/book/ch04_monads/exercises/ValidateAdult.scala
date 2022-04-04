package book.ch04_monads.exercises

import cats.MonadError
import cats.syntax.applicative._
import cats.syntax.applicativeError._

import scala.util.Try

object ValidateAdult extends App {
  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] =
    me.ensure(age.pure)(new IllegalArgumentException("Age must be greater than or equal to 18"))(_ >= 18)

  def validateAdult2[F[_] : MonadError[*[_], Throwable]](age: Int): F[Int] =
    if (age >= 18) age.pure[F]
    else new IllegalArgumentException("Age must be greater than or equal to 18").raiseError[F, Int]

  println(validateAdult[Try](18))
  println(validateAdult[Try](17))

  type ExceptionOr[A] = Either[Throwable, A]
  println(validateAdult[ExceptionOr](-73))

  println(validateAdult2[Try](18))
  println(validateAdult2[Try](17))

  println(validateAdult2[ExceptionOr](-73))
}
