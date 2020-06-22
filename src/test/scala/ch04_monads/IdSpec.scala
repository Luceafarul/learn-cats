package ch04_monads

import org.scalatest.{Matchers, WordSpec}

class IdSpec extends WordSpec with Matchers {
  "Id" should {
    "return wrapped value" in {
        val id = Id.pure(7)
        id shouldBe Id(7)
    }
  }
}
