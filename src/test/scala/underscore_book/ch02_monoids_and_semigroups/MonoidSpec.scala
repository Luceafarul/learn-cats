package underscore_book.ch02_monoids_and_semigroups

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MonoidSpec extends AnyWordSpec with Matchers {
  "Monoid Set" should {
    "return union of two Int ets" in {
      import book.ch02_monoids_and_semigroups.Monoid
      import Monoid.Instances.unionMonoid

      val intSetMonoid = Monoid[Set[Int]]
      val set01 = Set(1, 2, 3)
      val set02 = Set(2, 3, 4, 5)

      val result = intSetMonoid.combine(set01, set02)
      result should contain theSameElementsAs Set(1, 2, 3, 4, 5)
      result should have size 5
    }

    "return union of two String sets" in {
      import book.ch02_monoids_and_semigroups.Monoid
      import Monoid.Instances.unionMonoid

      val stringSetMonoid = Monoid[Set[String]]
      val set01 = Set("A", "B", "C")
      val set02 = Set("B", "C", "D")

      val result = stringSetMonoid.combine(set01, set02)
      result should contain theSameElementsAs Set("A", "B", "C", "D")
      result should have size 4
    }
  }
}
