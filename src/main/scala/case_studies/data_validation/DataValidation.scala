package case_studies.data_validation

import cats.data.Validated
import cats.data.Validated._
import cats.kernel.Semigroup
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.semigroup._

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

sealed trait Check[E, A] {
  import Check._

  def and(that: Check[E, A]): Check[E, A] = And(this, that)

  def or(that: Check[E, A]): Check[E, A] = Or(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
    case Pure(func) => func(a)
    case And(left, right) => (left(a), right(a)).mapN((_, _) => a)
    case Or(left, right) => left(a).findValid(right(a))
  }
}

object Check {
  final case class Or[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]

  final case class And[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]

  final case class Pure[E, A](func: A => Validated[E, A]) extends Check[E, A]

  def pure[E, A](f: A => Validated[E, A]): Check[E, A] = Pure(f)
}
