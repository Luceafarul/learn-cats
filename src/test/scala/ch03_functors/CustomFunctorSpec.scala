package ch03_functors

import cats.Functor
import cats.syntax.functor._
import org.scalatest.{WordSpec, Matchers}

class CustomFunctorSpec extends WordSpec with Matchers {
  "CustomFunctor" should {
    "add 1 to value in the Box" in {
      val boxWithInt = Box(13)
      boxWithInt.map(value => value + 1) shouldBe Box(14)
    }
  }
}
