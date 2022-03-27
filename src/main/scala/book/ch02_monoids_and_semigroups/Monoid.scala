package book.ch02_monoids_and_semigroups

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid

  object Instances {
    // Boolean monoid
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

    // Set monoid
    implicit def unionMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
      def combine(a: Set[A], b: Set[A]): Set[A] = a union b
      def empty: Set[A] = Set.empty[A]
    }

    implicit def symmetricDiffMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
      def combine(a: Set[A], b: Set[A]): Set[A] = (a diff b) union (b diff a)
      def empty: Set[A] = Set.empty[A]
    }

    // Semigroup because does not have an identity function for intersection
    implicit def intersectSemigroup[A]: Semigroup[Set[A]] = new Semigroup[Set[A]] {
      def combine(a: Set[A], b: Set[A]): Set[A] = a intersect b
    }
  }
}
