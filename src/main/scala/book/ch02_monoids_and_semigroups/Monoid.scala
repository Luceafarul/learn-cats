package book.ch02_monoids_and_semigroups

trait Semigroup[A] {
  def combine(a: A, b: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid
}
