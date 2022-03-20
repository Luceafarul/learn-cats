package book.ch02_monoids_and_semigroups.super_adder

import cats.Monoid
import cats.instances.int._
import cats.instances.option._
import cats.syntax.semigroup._

case class Order(totalCost: Double, quantity: Double)

object Order {
  implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
    def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)

    def empty: Order = Order(0.0, 0.0)
  }
}

object SuperAdder {
  def add[A](items: List[A])(implicit monoid: Monoid[A]): A = items.foldRight(Monoid.empty[A])(_ |+| _)
}
