package book.case_studies.data_validation

import cats.Monad
import cats.data.Kleisli
import cats.data.Validated
import cats.data.Validated._
import cats.kernel.Semigroup
import cats.instances.either._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.semigroup._
import cats.syntax.validated._

final case class CheckF[E, A](func: A => Either[E, A]) {
  def apply(a: A): Either[E, A] = func(a)

  def and(that: CheckF[E, A])(implicit s: Semigroup[E]): CheckF[E, A] = CheckF {
    a =>
      (this(a), that(a)) match {
        case (Left(e1), Left(e2)) => (e1.combine(e2)).asLeft
        case (Left(e), Right(_))  => e.asLeft
        case (Right(_), Left(e))  => e.asLeft
        case (Right(_), Right(_)) => a.asRight
      }
  }
}

sealed trait Predicate[E, A] {
  import Predicate._

  def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)

  def run(a: A)(implicit s: Semigroup[E]): Either[E, A] = apply(a).toEither

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
    case Pure(func)       => func(a)
    case And(left, right) => (left(a), right(a)).mapN((_, _) => a)
    case Or(left, right)  => left(a).findValid(right(a))
  }
}

object Predicate {
  final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A])
      extends Predicate[E, A]

  final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A])
      extends Predicate[E, A]

  final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]

  def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] = Pure(f)

  def lift[E, A](error: E, f: A => Boolean): Predicate[E, A] =
    Pure(a => if (f(a)) a.valid else error.invalid)
}

object UserValidation {
  import cats.data.{NonEmptyList, Validated}

  type Errors = NonEmptyList[String]

  type Result[A] = Either[Errors, A]

  type Check[A, B] = Kleisli[Result, A, B]

  // Create a check from a function:
  def check[A, B](func: A => Result[B]): Check[A, B] = Kleisli(func)

  // Create a check from Predicate:
  def checkPredicate[A](pred: Predicate[Errors, A]): Check[A, A] = Kleisli[Result, A, A](pred.run)

  def error(error: String): NonEmptyList[String] = NonEmptyList(error, Nil)

  def longerThan(n: Int): Predicate[Errors, String] = Predicate.lift(
    error(s"Must be longer than $n characters"),
    s => s.length > n
  )

  val alphanumeric: Predicate[Errors, String] = Predicate.lift(
    error(s"Must be all alphanumeric characters"),
    s => s.forall(_.isLetterOrDigit)
  )

  def contains(char: Char): Predicate[Errors, String] = Predicate.lift(
    error(s"Must contain the character: $char"),
    s => s.contains(char)
  )

  def containsOnce(char: Char): Predicate[Errors, String] = Predicate.lift(
    error(s"Must contain the character: $char only once"),
    s => s.filter(_ == char).size == 1
  )

  val usernameValidator: Check[String, String] =
    checkPredicate(longerThan(4).and(alphanumeric))

  val splitEmail: Check[String, (String, String)] = check(email =>
    email.split('@') match {
      case Array(name, domain) => Right((name, domain))
      case _ => Left(error("Must contain a single @ char"))
    }
  )

  val checkLeft: Check[String, String] = checkPredicate(longerThan(0))

  val checkRight: Check[String, String] = checkPredicate(longerThan(3).and(contains('.')))

  val joinEmail: Check[(String, String), String] = check {
    case (left, right) => (checkLeft(left), checkRight(right)).mapN(_ + "@" + _)
  }

  val emailValidator: Check[String, String] = splitEmail andThen joinEmail

  final case class User(name: String, email: String)

  // TODO: why errors does not accumulate into List?
  def createUser(name: String, email: String): Either[Errors, User] = 
    (usernameValidator.run(name), emailValidator.run(email)).mapN(User)
}
