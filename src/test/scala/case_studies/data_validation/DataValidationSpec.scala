package case_studies.data_validation

import cats.data.Validated
import cats.data.Validated._
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

  "Check" should {
    "validate and combine value" in {
      val a: Check[List[String], Int] = Check.pure { v =>
        if (v > 2) Valid(v)
        else Invalid(List("Must be > 2"))
      }

      val b: Check[List[String], Int] = Check.pure { v =>
        if (v < -2) Valid(v)
        else Invalid(List("Must be < -2"))
      }

      val check: Check[List[String], Int] = a.and(b)

      check(5) shouldBe Invalid(List("Must be < -2"))
      check(0) shouldBe Invalid(List("Must be > 2", "Must be < -2"))
    }
  }
}
