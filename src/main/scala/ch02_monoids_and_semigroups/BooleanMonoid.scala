package ch02_monoids_and_semigroups

import Monoid._

object BooleanMonoid {
  implicit val andMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def combine(a: Boolean, b: Boolean): Boolean = a && b

    def empty: Boolean = true
  }

  implicit val orMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def combine(a: Boolean, b: Boolean): Boolean = a || b

    def empty: Boolean = false
  }

  implicit val eitherMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def combine(a: Boolean, b: Boolean): Boolean = (a && !b) || (!a && b)

    def empty: Boolean = false
  }

  implicit val xNorMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def combine(a: Boolean, b: Boolean): Boolean = (!a || b) && (a || !b)

    def empty: Boolean = true
  }
}
