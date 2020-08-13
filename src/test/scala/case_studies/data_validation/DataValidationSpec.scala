package case_studies.data_validation

import cats.data.Validated
import cats.data.Validated._
import cats.data.NonEmptyList
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

  "Predicate" should {
    import Predicate._

    "validate and combine an errors with and function" in {
      val a: Predicate[List[String], Int] = Pure { v =>
        if (v > 2) Valid(v)
        else Invalid(List("Must be > 2"))
      }

      val b: Predicate[List[String], Int] = Pure { v =>
        if (v < -2) Valid(v)
        else Invalid(List("Must be < -2"))
      }

      val check: Predicate[List[String], Int] = a.and(b)

      check(5) shouldBe Invalid(List("Must be < -2"))
      check(0) shouldBe Invalid(List("Must be > 2", "Must be < -2"))
    }

    "validate and combine values with and function" in {
      val a: Predicate[List[String], Int] = Pure { v =>
        if (v > 2) Valid(v)
        else Invalid(List("Must be > 2"))
      }

      val b: Predicate[List[String], Int] = Pure { v =>
        if (v < 12) Valid(v)
        else Invalid(List("Must be < 12"))
      }

      val check: Predicate[List[String], Int] = a.and(b)

      check(7) shouldBe Valid(7)
    }

    "validate and combine values or an errors with or function" in {
      val a: Predicate[List[String], Int] = Pure { v =>
        if (v > 2) Valid(v)
        else Invalid(List("Must be > 2"))
      }

      val b: Predicate[List[String], Int] = Pure { v =>
        if (v < -2) Valid(v)
        else Invalid(List("Must be < -2"))
      }

      val check: Predicate[List[String], Int] = a.or(b)

      check(5) shouldBe Valid(5)
      check(0) shouldBe Invalid(List("Must be > 2", "Must be < -2"))
    }
  }

  "Check" should {
    import Predicate._

    "transform Check value with map if it's valid or return error" in {
      val a: Predicate[List[String], Int] = Pure { v =>
        if (v > 2) Valid(v)
        else Invalid(List("Must be > 2"))
      }

      val check: Check[List[String], Int, Int] = Check(a).map((x: Int) => x * 2)

      check(5) shouldBe Valid(10)
      check(1) shouldBe Invalid(List("Must be > 2"))
    }

    "transform Check value with flatMap if it's valid or return error" in {
      val a: Predicate[List[String], Int] = Pure { v =>
        if (v > 2) Valid(v)
        else Invalid(List("Must be > 2"))
      }

      val b: Predicate[List[String], Int] = Pure { v =>
        if (v < 10) Valid(v)
        else Invalid(List("Must be < 10"))
      }

      val check1: Check[List[String], Int, Int] = Check(a)
      val check2: Check[List[String], Int, Int] = Check(b)

      val check: Check[List[String], Int, Int] =
        check1.flatMap((x: Int) => check2)

      check(5) shouldBe Valid(5)
      check(11) shouldBe Invalid(List("Must be < 10"))
    }

    "apply two Checks with andThen or return error" in {
      val a: Predicate[List[String], Int] = Pure { v =>
        if (v > 2) Valid(v)
        else Invalid(List("Must be > 2"))
      }

      val b: Predicate[List[String], Int] = Pure { v =>
        if (v < 10) Valid(v)
        else Invalid(List("Must be < 10"))
      }

      val check1: Check[List[String], Int, Int] = Check(a)
      val check2: Check[List[String], Int, Int] = Check(b)

      val check: Check[List[String], Int, Int] = check1.andThen(check2)

      check(5) shouldBe Valid(5)
      check(1) shouldBe Invalid(List("Must be > 2"))
    }
  }

  "UserValidation" should {
    "validate user name correct" in {
      val validUsername = "Val1d"
      val invalidUsername1 = "no"
      val invalidUsername2 = "|!d"

      UserValidation.usernameValidator(validUsername) shouldBe Valid(validUsername)
      UserValidation.usernameValidator(invalidUsername1) shouldBe Invalid(NonEmptyList.of("Must be longer than 4 characters"))
      UserValidation.usernameValidator(invalidUsername2) shouldBe Invalid(
        NonEmptyList.of(
          "Must be longer than 4 characters",
          "Must be all alphanumeric characters"
        )
      )
    }
  }
}
