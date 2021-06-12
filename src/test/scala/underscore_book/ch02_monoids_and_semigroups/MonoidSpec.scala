package underscore_book.ch02_monoids_and_semigroups

import org.scalatest.{WordSpec, Matchers}

class MonoidSpec extends WordSpec with Matchers {
  "Monoid Set" should {
    "return union of two Int ets" in {
      import underscore_book.ch02_monoids_and_semigroups.Monoid
      import underscore_book.ch02_monoids_and_semigroups.SetMonoid.unionMonoid

      val intSetMonoid = Monoid[Set[Int]]
      val set01 = Set(1, 2, 3)
      val set02 = Set(2, 3, 4, 5)

      val result = intSetMonoid.combine(set01, set02)
      result should contain theSameElementsAs Set(1, 2, 3, 4, 5)
      result should have size 5
    }

    "return union of two String sets" in {
      import underscore_book.ch02_monoids_and_semigroups.Monoid
      import underscore_book.ch02_monoids_and_semigroups.SetMonoid.unionMonoid

      val stringSetMonoid = Monoid[Set[String]]
      val set01 = Set("A", "B", "C")
      val set02 = Set("B", "C", "D")

      val result = stringSetMonoid.combine(set01, set02)
      result should contain theSameElementsAs Set("A", "B", "C", "D")
      result should have size 4
    }
  }
}
