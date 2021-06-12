package underscore_book.case_studies.data_validation

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

  "UserValidation" should {
    "validate user name correct" in {
      val validUsername = "Val1d"
      val invalidUsername1 = "no"
      val invalidUsername2 = "|!d"

      UserValidation.usernameValidator(validUsername) shouldBe Right(
        validUsername
      )
      UserValidation.usernameValidator(invalidUsername1) shouldBe Left(
        NonEmptyList.of("Must be longer than 4 characters")
      )
      UserValidation.usernameValidator(invalidUsername2) shouldBe Left(
        NonEmptyList.of(
          "Must be longer than 4 characters",
          "Must be all alphanumeric characters"
        )
      )
    }

    "validate user email correct" in {
      val validEmail = "hello@dark.ness"
      val invalidEmail01 = "@test.com"
      val invalidEmail02 = "inv@t.c"
      val invalidEmail03 = "invt.c"
      val invalidEmail04 = "inv@tc"

      UserValidation.emailValidator(validEmail) shouldBe Right(validEmail)
      UserValidation.emailValidator(invalidEmail01) shouldBe Left(
        NonEmptyList.of("Must be longer than 0 characters")
      )
      UserValidation.emailValidator(invalidEmail02) shouldBe Left(
        NonEmptyList.of("Must be longer than 3 characters")
      )
      UserValidation.emailValidator(invalidEmail03) shouldBe Left(
        NonEmptyList.of("Must contain a single @ char")
      )
      UserValidation.emailValidator(invalidEmail04) shouldBe Left(
        NonEmptyList.of(
          "Must be longer than 3 characters",
          "Must contain the character: ."
        )
      )
    }

    "user validate correctly" in {
      import UserValidation.User

      val marcus = UserValidation.createUser("Marcus", "maarcus@test.com")
      val invalidUser =
        UserValidation.createUser("", "ivalid@user.com@user.com")

      marcus shouldBe Right(User("Marcus", "maarcus@test.com"))
      invalidUser shouldBe Left(
        NonEmptyList.of(
          "Must be longer than 4 characters",
          "Must contain a single @ char"
        )
      )
    }
  }
}
