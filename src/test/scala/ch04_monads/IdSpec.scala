package ch04_monads

import cats.Id
import org.scalatest.{Matchers, WordSpec}

class IdSpec extends WordSpec with Matchers {
  "Id" should {
    "return wrapped value" in {
      Id.pure(7) shouldBe 7
    }

    "return correct value after apply map" in {
      val id = Id.pure(7)
      val res = Id.map(id)(n => n * 2)

      res shouldBe 14
    }

    "return correct value after apply flatMap" in {
      val id = Id.pure("hello")
      val res = Id.flatMap(id)(ch => Id.pure(ch * 2))

      res shouldBe "hellohello"
    }
  }
}
