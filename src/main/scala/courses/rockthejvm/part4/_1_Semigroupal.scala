package courses.rockthejvm.part4

import cats.Monad

object _1_Semigroupal extends App {

  import cats.Semigroupal

  val optinoSemigroupal = Semigroupal[Option]

  val aSomeTupled = optinoSemigroupal.product(Some(123), Some("Hello"))
  val aNoneTupled = optinoSemigroupal.product(Some(123), None)

  println(s"A some tupled: $aSomeTupled")
  println(s"A none tupled: $aNoneTupled")

  val aListTupled = Semigroupal[List].product(List(1, 2), List("A", "B", "C"))

  println(s"A list tupled: $aListTupled")

  // Exercise:
  import cats.implicits._
  def productWithMonads[F[_] : Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a, b)))

  println(productWithMonads(List(1, 2, 3), List("a", "b")))



  // MONADS extends SEMIGROUPALS


  // Example: Validated
  import cats.data.Validated

  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal = Semigroupal[ErrorsOr] // Requires implicit Semigroup[List[_]]
  val invalidsCombinations = validatedSemigroupal.product(
    Validated.invalid(List("Something went wrong", "Error with API")),
    Validated.invalid(List("Connection failed")),
  )
  println(invalidsCombinations)

  type EitherErrorOr[T] = Either[List[String], T]
  val eitherSemigroupal = Semigroupal[EitherErrorOr]
  val eitherCombinations = eitherSemigroupal.product(
    Left(List("Invalid request", "Missed parameter 'name'")),
    Left(List("Bad request"))
  )
  println(eitherCombinations)

  // Exercise:
  // Define a Semigroupal[List] which does a zip
  val zippedSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] =
      fa.zip(fb)
  }
  val aListZipped = zippedSemigroupal.product(List(1, 2), List("A", "B", "C"))

  println(s"A list zipped: $aListZipped")
}
