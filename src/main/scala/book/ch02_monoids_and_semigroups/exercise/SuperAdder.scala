package book.ch02_monoids_and_semigroups.exercise

import cats.Monoid
import cats.syntax.semigroup._

final case class Order(totalCost: Double, quantity: Double)

object Order {
  implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
    def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
    def empty: Order = Order(0.0, 0.0)
  }
}

object SuperAdder {
  def add[A](items: List[A])(implicit monoid: Monoid[A]): A =
    items.foldRight(Monoid.empty[A])(_ |+| _)
}
