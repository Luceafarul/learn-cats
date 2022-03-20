package underscore_book.ch02_monoids_and_semigroups

import book.ch02_monoids_and_semigroups.super_adder.{Order, SuperAdder}
import cats.Monoid
import cats.instances.int._
import cats.instances.option._
import cats.syntax.semigroup._
import org.scalatest.{WordSpec, Matchers}

class SuperAdderSpec extends WordSpec with Matchers {
  "SuperAdder" should {
    "add all elements of int list" in {
      val list = List(1, 2, 3, 4, 5)

      SuperAdder.add(list) shouldBe 15
    }

    "add all elements of option int list" in {
      val list = List(Some(1), Some(2), Some(3), None, Some(4), Some(5))

      SuperAdder.add(list) shouldBe Some(15)
    }

    "add all elements of Order list" in {
      val list = List(Order(12.99, 1), Order(18.98, 2), Order(27.66, 3))

      SuperAdder.add(list) shouldBe Order(59.63, 6.0)
    }
  }
}
