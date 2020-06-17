package ch02_monoids_and_semigroups.super_adder

import cats.Monoid
import cats.instances.int._
import cats.instances.option._
import cats.syntax.semigroup._

case class Order(totalCost: Double, quantity: Double)

// TODO: monoid for order???
object Order {
  implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
    def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)

    def empty: Order = Order(0.0, 0.0)
  }
}

object SuperAdder {
  // TODO:
  //  1. Generalize this methods
  //  2. Use Monoid.empty for init element of foldRight
  def add(items: List[Int]): Int = items.foldRight(Monoid.empty[Int])(_ |+| _)

  def add(items: List[Option[Int]]): Option[Int] = items.foldRight(Monoid.empty[Option[Int]])(_ |+| _)

  def add(items: List[Order]): Order = items.foldRight(Monoid.empty[Order])(_ |+| _)

  def add[A](items: List[A])(implicit monoid: Monoid[A]): A = items.foldRight(Monoid.empty[A])(_ |+| _)
}
