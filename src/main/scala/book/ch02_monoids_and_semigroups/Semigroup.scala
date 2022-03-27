package book.ch02_monoids_and_semigroups

trait Semigroup[A] {
  def combine(a: A, b: A): A
}
