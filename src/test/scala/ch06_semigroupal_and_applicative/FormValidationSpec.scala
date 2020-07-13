package ch06_semigroupal_and_applicative

import cats.data.Validated
import org.scalatest.{WordSpec, Matchers}
import cats.data.Validated.Valid
import cats.data.Validated.Invalid

class FormValidationSpec extends WordSpec with Matchers {
  import FormValidation._

  "FormValidation" should {
    "getValue return value or error message if key does not exist" in {
      val params = Map("keyOne" -> "value one")

      getValue("keyOne", params) shouldBe Right("value one")
      getValue("notExistKey", params) shouldBe Left(
        List("Param notExistKey does not exist")
      )
    }

    "parseInt return parsed Int value or error message" in {
      parseInt("17") shouldBe Right(17)
      parseInt("17th") shouldBe Left(List("Can not parse 17th to Int"))
    }

    "nonBlank return value if its or non empty/blank String or error message if not" in {
      nonBlank("Marcus") shouldBe Right("Marcus")
      nonBlank("   ") shouldBe Left(List("name field should be non empty"))
      nonBlank("") shouldBe Left(List("name field should be non empty"))
    }

    "nonNegative return Int value if is grether than 0 or error message if not" in {
      nonNegative(13) shouldBe Right(13)
      nonNegative(-21) shouldBe Left(List("age should be greater than zero"))
    }

    "readName return name from FormParams or error message if message does not exist in FormParams or its empty" in {
      val params = Map("name" -> "Marcus")
      val emtpyName = Map("name" -> " ")
      val wrongParams = Map("firstName" -> "Marcus")

      readName(params) shouldBe Right("Marcus")
      readName(emtpyName) shouldBe Left(List("name field should be non empty"))
      readName(wrongParams) shouldBe Left(List("Param name does not exist"))
    }

    "readAge return age from FormParams or error messages" in {
      val params = Map("age" -> "17")
      val emtpyAge = Map("age" -> "")
      val wrongValue = Map("age" -> "17th")
      val wrongParams = Map("user_age" -> "17")

      readAge(params) shouldBe Right(17)
      readAge(emtpyAge) shouldBe Left(List("name field should be non empty"))
      readAge(wrongValue) shouldBe Left(List("Can not parse 17th to Int"))
      readAge(wrongParams) shouldBe Left(List("Param age does not exist"))
    }

    "readUser return User when all params exist and valid" in {
      val validForm = Map("name" -> "Marcus", "age" -> "73")

      readUser(validForm) shouldBe Valid(User("Marcus", 73))
    }

    "readUser return error messsages when form params invalid" in {
      val invalidForm = Map("name" -> "", "age" -> "73a")

      readUser(invalidForm) shouldBe Invalid(List("name field should be non empty", "Can not parse 73a to Int"))
    }
  }
}
