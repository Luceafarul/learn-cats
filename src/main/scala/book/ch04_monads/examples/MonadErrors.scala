package book.ch04_monads.examples

import cats.MonadError
import cats.instances.either._

object MonadErrors extends App {
  type ErrorOr[A] = Either[String, A]

  val monadError = MonadError[ErrorOr, String]

  val success = monadError.pure(73)

  val failure = monadError.raiseError("fail...")

  println(success)
  println(failure)

  val handleErrorWith = monadError.handleErrorWith(failure) {
    case "fail..." => monadError.pure("It's ok")
    case _ => monadError.raiseError("It's not ok")
  }
  println(handleErrorWith)

  val handleError = monadError.handleError(failure) {
    case "fail..." => 73
    case _ => -1
  }
  println(handleError)

  println(monadError.ensure(success)("Number is too low!")(_ > 1000))

  import cats.syntax.applicative._ // for pure
  import cats.syntax.applicativeError._ // for raiseError etc
  import cats.syntax.monadError._ // for raiseError etc

  val successTwo = 73.pure[ErrorOr]
  val failureTwo = "fail".raiseError[ErrorOr, Int]

  failureTwo.handleErrorWith {
    case "fail" => 37.pure
    case _ => "It's not ok".raiseError
  }
  println(failureTwo)

  println(successTwo.ensure("Number is too low!")(_ > 1000))
}
