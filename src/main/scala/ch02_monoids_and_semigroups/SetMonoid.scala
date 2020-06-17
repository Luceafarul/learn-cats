package ch02_monoids_and_semigroups

import Monoid._

object SetMonoid {
  implicit def unionMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def combine(a: Set[A], b: Set[A]): Set[A] = a union b
    def empty: Set[A] = Set.empty[A]
  }

  implicit def symmectricDiffMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
      def combine(a: Set[A], b: Set[A]): Set[A] = (a diff b) union (b diff a)
      def empty: Set[A] = Set.empty[A]
  }

  // Semigroup because does not have an identity function for intersection
  implicit def intesectSemigroup[A]: Semigroup[Set[A]] = new Semigroup[Set[A]] {
      def combine(a: Set[A], b: Set[A]): Set[A] = a intersect b
  }
}
