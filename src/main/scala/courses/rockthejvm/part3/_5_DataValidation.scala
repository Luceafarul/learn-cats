package courses.rockthejvm.part3

import cats.kernel.Semigroup

import scala.annotation.tailrec
import scala.util.Try

object _5_DataValidation extends App {

  import cats.data.Validated

  val aValidValue: Validated[String, Int] = Validated.valid(73)
  val anInvalidValue: Validated[String, Int] = Validated.invalid("Something went wrong!")
  val aTest: Validated[String, Int] = Validated.cond(37 > 13, 73, "Meaning of life is too small")

  println(aValidValue)
  println(anInvalidValue)
  println(aTest)

  // Exercise: use Either
  // - n must be a prime
  // - n must be non-negative
  // - n <= 100
  // - n must be even
  def isPrime(n: Int): Boolean = {
    @tailrec
    def loop(i: Int): Boolean =
      if (i <= 1) true
      else n % i != 0 && loop(i - 1)

    if (n == 0 || n == 1 || n == -1) false
    else loop(math.abs(n / 2))
  }

  def testNumber(n: Int): Either[List[String], Int] = {
    var errors = List.empty[String]

    if (!isPrime(n)) errors = errors :+ "n must be a prime"
    if (n < 0) errors = errors :+ "n must be non-negative"
    if (n > 100) errors = errors :+ "n must be n <= 100"
    if (n % 2 == 0) Right(n)
    else {
      errors = errors :+ "n must be even"
      Left(errors)
    }
  }

  def solutionTestNumber(n: Int): Either[List[String], Int] = {
    val isNotEven: List[String] = if (n % 2 == 0) List() else List("Number must be even")
    val isNegative: List[String] = if (n >= 0) List() else List("Number must be non-negative")
    val isToBig: List[String] = if (n <= 100) List() else List("Number must be less than or equals to 100")
    val isNotPrime: List[String] = if (isPrime(n)) List() else List("Number must be a prime")

    if (n % 2 == 0 && n >= 0 && n <= 100 && isPrime(n)) Right(n)
    else Left(isNotEven ++ isNegative ++ isToBig ++ isNotPrime)
  }

  println(testNumber(2))
  println(testNumber(113))

  println(solutionTestNumber(2))
  println(solutionTestNumber(113))

  implicit val combineIntMax: Semigroup[Int] = Semigroup.instance[Int](math.max)

  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated.cond(n % 2 == 0, n, List("Number must be even"))
      .combine(Validated.cond(n >= 0, n, List("Number must be non-negative")))
      .combine(Validated.cond(n <= 100, n, List("Number must be less than or equals to 100")))
      .combine(Validated.cond(isPrime(n), n, List("Number must be a prime")))

  println(validateNumber(2))
  println(validateNumber(113))

  // Chain
  println(aValidValue.andThen(_ => anInvalidValue))

  // Test a valid value
  println(aValidValue.ensure(List("Something went wrong!"))(_ % 2 == 0))

  // Transformation
  println(aValidValue.map(_ * 2))
  println(aValidValue.leftMap(_.length))
  println(anInvalidValue.leftMap(_.length))
  println(aValidValue.bimap(_.length, _ * 2))
  println(anInvalidValue.bimap(_.length, _ * 2))

  // Interop with stdlib
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(37))
  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(None, List("Noting present here"))
  val tryToValidated: Validated[Throwable, Int] = Validated.fromTry(Try("some".toInt))

  println(s"Either to Validated: $eitherToValidated")
  println(s"Option to Validated: $optionToValidated")
  println(s"Try to Validated: $tryToValidated")

  // and backwards
  println(aValidValue.toEither)
  println(anInvalidValue.toEither)
  println(aValidValue.toOption)
  println(anInvalidValue.toOption)

  // Exercise:
  object FormValidation {
    type Form = Map[String, String]
    type FormValidation[A] = Validated[List[String], A]

    // Fields:
    // - name
    // - email
    // - password

    // Rules:
    // - name, email and password MUST be specified
    // - name must not be blank
    // - email must have "@"
    // - password must have >= 10 characters

    def validateForm(form: Form): FormValidation[String] = {
      val name = getValue(form, "name")
        .andThen(name => Validated.cond(!name.isBlank, name, List("name must not be blank")))
      val email = getValue(form, "email")
        .andThen(email => Validated.cond(email.contains("@"), email, List("email must have '@'")))
      val password = getValue(form, "password")
        .andThen(password => Validated.cond(password.length >= 10, password, List("password must have >= 10 characters")))

      name
        .combine(email)
        .combine(password)
        .map(_ => "Success")
    }

    private def getValue(form: Form, name: String): FormValidation[String] =
      Validated.fromOption(form.get(name), List(s"$name field should exist"))
  }

  val successForm = Map(
    "name" -> "Yaroslav",
    "email" -> "test@email.com",
    "password" -> "Hello12345",
  )
  println(FormValidation.validateForm(successForm))
}
