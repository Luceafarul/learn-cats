package courses.rockthejvm.part4

import cats.{Applicative, Monad}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

object _5_ErrorHandling extends App {
  // Level 1: try/catch blocks
  // Level 2: using Try
  // Level 3: pure FP with Cats

  trait _ApplicativeError[F[_], E] extends Applicative[F] {
    // pure from Applicative
    def raiseError[A](e: E): F[A]
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
    def handleError[A](fa: F[A])(f: E => A): F[A] =
      handleErrorWith(fa)(e => pure(f(e)))
  }

  trait _MonadError[F[_], E] extends _ApplicativeError[F, E] with Monad[F] {
    def ensure[A](fa: F[A])(error: E)(predicate: A => Boolean): F[A]
  }

  import cats.MonadError

  type ErrorOr[A] = Either[String, A]

  val monadErrorEither = MonadError[ErrorOr, String]
  val success = monadErrorEither.pure(73) // Either[String, Int] == Right(73)
  val failure = monadErrorEither.raiseError[Int]("Something went wrong") // Either[String, Int] == Left("Something went wrong"

  println("Recover:")
  val handledError = monadErrorEither.handleError(failure) {
    case "Fail" => -1
    case _      => 0
  }
  println(handledError)

  val handledErrorWith = monadErrorEither.handleErrorWith(failure) {
    case "Fail" => monadErrorEither.pure(-1)
    case _      => Left("Unknown fail")
  }
  println(handledErrorWith)

  println("\nFilter:")
  println(monadErrorEither.ensure(success)("Number less than 100")(_ >= 100))
  println(monadErrorEither.ensure(success)("Number less than 50")(_ >= 50))

  println("\nTry and Future:")
  val exception = new RuntimeException("Really bad")
  val pureException = MonadError[Try, Throwable].raiseError(exception)
  println(s"Try: $pureException")
  val futureException = MonadError[Future, Throwable].raiseError(exception)
  futureException.onComplete(result => println(s"Future: $result"))

  // Applicative => ApplicativeError
  import cats.data.Validated
  import cats.ApplicativeError

  type ErrorsOr[T] = Validated[List[String], T]

  // Extensions methods:
  import cats.syntax.applicative._
  import cats.syntax.applicativeError._

  val extendedSuccess = 73.pure[ErrorsOr]
  val extendedError = List("API Error").raiseError[ErrorsOr, Int]

  println("\nExtensions methods:")
  println(extendedSuccess)
  println(extendedError)

  val recoveredError = extendedError.recover {
    case _ => 71
  }
  println(recoveredError)

  val checkedSuccess = extendedSuccess.ensure("Number less than 100")(_ >= 100)
  println(checkedSuccess)
}
