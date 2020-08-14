package case_studies.data_validation

import cats.Monad
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

sealed trait Check[E, A, B] {
  import Check._

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]

  def map[C](f: B => C): Check[E, A, C] = Map[E, A, B, C](this, f)

  def flatMap[C](f: B => Check[E, A, C]): Check[E, A, C] = FlatMap[E, A, B, C](this, f)

  def andThen[C](that: Check[E, B, C]): Check[E, A, C] = AndThen[E, A, B, C](this, that)
}

object Check {
  final case class Map[E, A, B, C](check: Check[E, A, B], f: B => C) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] = check(a).map(f)
  }

  final case class FlatMap[E, A, B, C](check: Check[E, A, B], f: B => Check[E, A, C]) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).withEither(e => e.flatMap(b => f(b)(a).toEither))
  }

  final case class AndThen[E, A, B, C](check1: Check[E, A, B], check2: Check[E, B, C]) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check1(a).withEither(e => e.flatMap(b => check2(b).toEither))
  }

  final case class Pure[E, A, B](f: A => Validated[E, B]) extends Check[E, A, B] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B] = f(a)
  }

  final case class PurePredicate[E, A](p: Predicate[E, A]) extends Check[E, A, A] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = p(a)
  }

  def apply[E, A](p: Predicate[E, A]): Check[E, A, A] = PurePredicate(p)

  def apply[E, A, B](f: A => Validated[E, B]): Check[E, A, B] = Pure(f)
}

object UserValidation {
  import cats.data.{NonEmptyList, Validated}

  type Errors = NonEmptyList[String]

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

  val usernameValidator: Check[Errors, String, String] =
    Check(longerThan(4).and(alphanumeric))

  val splitEmail: Check[Errors, String, (String, String)] = Check(email =>
    email.split('@') match {
      case Array(name, domain) => (name, domain).validNel[String]
      case _ => "Must contain a single @ char".invalidNel[(String, String)]
    }
  )

  val checkLeft: Check[Errors, String, String] = Check(longerThan(0))

  val checkRight: Check[Errors, String, String] = Check(longerThan(3).and(contains('.')))

  val joinEmail: Check[Errors, (String, String), String] = Check {
    case (left, right) => (checkLeft(left), checkRight(right)).mapN(_ + "@" + _)
  }

  val emailValidator: Check[Errors, String, String] = splitEmail andThen joinEmail
}
