package underscore_book.ch04_monads

import cats.Monad
import scala.annotation.tailrec

/**
  * To define a Monad for a custom type, need provid implementations of three methods:
  * flatMap,
  * pure,
  * and tailRecM - is an optimisation used in Cats to limit the amount of stack space
  * consumed by nested calls to flatMap.
  */
object OptionMonad {
  val optionMonad = new Monad[Option] {
    def flatMap[A, B](option: Option[A])(f: A => Option[B]): Option[B] =
      option.flatMap(f)

    def pure[A](value: A): Option[A] = Some(value)

    @tailrec
    def tailRecM[A, B](value: A)(f: A => Option[Either[A, B]]): Option[B] =
      f(value) match {
        case None           => None
        case Some(Left(a))  => tailRecM(a)(f)
        case Some(Right(b)) => Some(b)
      }
  }
}
