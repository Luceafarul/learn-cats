package case_studies.data_validation

import cats.syntax.either._
import cats.instances.list._
import org.scalatest.{WordSpec, Matchers}

class DataValidationSpec extends WordSpec with Matchers {
  "CheckF" should {
    "validate and combine value" in {
      val a: CheckF[List[String], Int] = CheckF { v =>
        if (v > 2) v.asRight
        else List("Must be > 2").asLeft
      }

      val b: CheckF[List[String], Int] = CheckF { v =>
        if (v < -2) v.asRight
        else List("Must be < -2").asLeft
      }

      val check: CheckF[List[String], Int] = a.and(b)

      check(5) shouldBe Either.left(List("Must be < -2"))
      check(0) shouldBe Either.left(List("Must be > 2", "Must be < -2"))
    }
  }
}
