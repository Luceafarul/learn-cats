package docs.doc10_validated

import cats.data.ValidatedNel
import cats.implicits.{catsSyntaxTuple3Semigroupal, catsSyntaxValidatedId}
import cats.syntax.either.catsSyntaxEither

import scala.util.Try

object EitherVsValidate extends App {
  sealed trait PersonErrorType
  case object NameInvalid extends PersonErrorType
  case object AgeInvalid extends PersonErrorType
  case object EmailInvalid extends PersonErrorType

  final case class PersonError(message: String, errorType: PersonErrorType)

  final case class Name(value: String)
  final case class Age(value: Int)
  final case class Email(value: String)
  final case class Person(name: Name, age: Age, email: Email)

  type ErrorOr[A] = Either[PersonError, A]

  def validateName(name: String): ErrorOr[Name] =
    if (name.headOption.exists(_.isUpper)) Right(Name(name))
    else Left(PersonError(s"Name is empty or does not start with an uppercase character: $name", NameInvalid))

  def validateAge(age: String): ErrorOr[Age] =
    for {
      numericAge <- Try(age.toInt).toEither.left.map(exception => PersonError(exception.getMessage, AgeInvalid))
      validAge <-
        if (numericAge <= 0 || numericAge >= 150)
          Left(PersonError(s"Age must be a number between 1-120: $numericAge", AgeInvalid))
        else Right(numericAge)
    } yield Age(validAge)

  def validateEmail(email: String): ErrorOr[Email] =
    if (email.isEmpty || !email.contains("@"))
      Left(PersonError(s"Email address is empty or does not contain an `@` symbol: $email", EmailInvalid))
    else Right(Email(email))

  def validatePerson(name: String, age: String, email: String): ErrorOr[Person] =
    for {
      validName <- validateName(name)
      validAge <- validateAge(age)
      validEmail <- validateEmail(email)
    } yield Person(validName, validAge, validEmail)

  // Is valid
  println(validatePerson("Benjamin Sisko", "50", "b.sisko@dsn.st"))
  // Not valid, but we got only first validation error (name is empty or does not start from uppercase)
  println(validatePerson("odo", "200", "odo.founder.net"))

  // If we wont to collect validation error we can use Validated data-type from cats
  // Old
  // type AllErrorsOr[A] = Validated[PersonError, A]

  //
  type AllErrorsOr[A] = ValidatedNel[PersonError, A]

  def validateNameV(name: String): AllErrorsOr[Name] =
    if (name.headOption.exists(_.isUpper)) Name(name).validNel
    else PersonError(s"Name is empty or does not start with an uppercase character: $name", NameInvalid).invalidNel

  def validateAgeV(age: String): AllErrorsOr[Age] = {
    val numericAge =
      Try(age.toInt).toEither.left.map(exception => PersonError(exception.getMessage, AgeInvalid)).toValidatedNel

    def validatedAge(age: Int): AllErrorsOr[Age] =
      if (age <= 0 || age >= 150) PersonError(s"Age must be a number between 1-120: $age", AgeInvalid).invalidNel
      else Age(age).validNel

    numericAge.andThen(validatedAge)
  }

  def validateEmailV(email: String): AllErrorsOr[Email] =
    if (email.isEmpty || !email.contains("@"))
      PersonError(s"Email address is empty or does not contain an `@` symbol: $email", EmailInvalid).invalidNel
    else Email(email).validNel

  def validatePersonV(name: String, age: String, email: String): AllErrorsOr[Person] =
    (validateNameV(name), validateAgeV(age), validateEmailV(email)).mapN(Person.apply)

  // Is valid
  println(validatePersonV("Benjamin Sisko", "50", "b.sisko@dsn.st"))
  // Not valid, validated errors are collected in NEL
  println(validatePersonV("odo", "200", "odo.founder.net"))
}
