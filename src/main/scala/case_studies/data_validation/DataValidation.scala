package case_studies.data_validation

import cats.kernel.Semigroup
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
