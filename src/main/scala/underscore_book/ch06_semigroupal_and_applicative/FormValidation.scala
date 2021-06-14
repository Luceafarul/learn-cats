package underscore_book.ch06_semigroupal_and_applicative

import cats.data.Validated
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.apply._

object FormValidation {

  type FormValidation[A] = Validated[List[String], A]
  type FieldValidation[A] = Either[List[String], A]

  final case class User(name: String, age: Int)

  def readUser(formParams: Map[String, String]): FormValidation[User] = (
    readName(formParams).toValidated,
    readAge(formParams).toValidated
  ).mapN(User.apply)

  def readName(formParams: Map[String, String]): FieldValidation[String] =
    getValue("name", formParams).flatMap(name => nonBlank(name))

  def readAge(formParams: Map[String, String]): FieldValidation[Int] =
    getValue("age", formParams)
      .flatMap(age => nonBlank(age))
      .flatMap(age => parseInt(age))
      .flatMap(age => nonNegative(age))

  def getValue(param: String, formParams: Map[String, String]): FieldValidation[String] =
    formParams.get(param) match {
      case Some(value) => Right(value)
      case None        => Left(List(s"Param $param does not exist"))
    }

  def parseInt(s: String): FieldValidation[Int] =
    try Right(s.toInt)
    catch { case _: NumberFormatException => Left(List(s"Can not parse $s to Int")) }

  def nonBlank(s: String): FieldValidation[String] =
    Right(s).ensure(List("name field should be non empty"))(_.nonEmpty)

  def nonNegative(n: Int): FieldValidation[Int] =
    Right(n).ensure(List("age should be greater than zero"))(n => n > 0)
}
