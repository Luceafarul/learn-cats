package courses.rockthejvm.part4

object _2_Applicative extends App {

  // Applicative = Functors + the pure method

  import cats.Applicative

  val listApplicative = Applicative[List]
  val aList = listApplicative.pure(3)
  println(aList)

  val optionApplicative = Applicative[Option]
  val anOption = optionApplicative.pure(13)
  println(anOption)

  import cats.syntax.applicative._

  val aSweetList = 123.pure[List]
  val aSweetOption = 321.pure[Option]

  println(aSweetList)
  println(aSweetOption)

  // Monad extends Applicative
  // Applicative extends Functors

  import cats.data.Validated

  type ErrorsOr[T] = Validated[List[String], T]

  val aValidValue: ErrorsOr[Int] = Validated.valid(73)
  val aModifiedValidated: ErrorsOr[Int] = aValidValue.map(_ + 1)
  val validatedApplicative = Applicative[ErrorsOr]

  println(aModifiedValidated)

  import cats.implicits._

  def productWithApplicative[F[_] : Applicative, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    Applicative[F].ap(fb.map(b => (a: A) => (a, b)))(fa)

  def productWithApplicativeSolution[F[_] : Applicative, A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
    val functionWrapper: F[B => (A, B)] = fa.map(a => (b: B) => (a, b))
    Applicative[F].ap(functionWrapper)(fb)
  }

  // Applicative has ap method -> def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
  // Applicative can implement product from Semigroupal
  // Applicative extends Semigroupal
}
