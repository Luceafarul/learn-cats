package docs.doc07_traverse

import cats.data.{Validated, ValidatedNel}
import cats.implicits._

object TraverseExample extends App {
  def parseIntEither(s: String): Either[NumberFormatException, Int] = {
    println(s"try to either $s")
    Either.catchOnly[NumberFormatException](s.toInt)
  }

  def parseIntValidated(s: String): ValidatedNel[NumberFormatException, Int] = {
    println(s"try to validate $s")
    Validated.catchOnly[NumberFormatException](s.toInt).toValidatedNel
  }

  val result01 = List("1", "2", "3").traverse(parseIntEither)
  println(result01)
  val result02 = List("1", "abc", "3").traverse(parseIntEither)
  println(result02)

  val result03 = List("1", "2", "3").traverse(parseIntValidated)
  println(result03)
  val result04 = List("1", "abc", "3").traverse(parseIntValidated)
  println(result04)

  // Playing with Reader
  // Reader[E, A] is a type alias for Kleisli[Id, E, A]
  // which is a wrapper around E => A

  println(List(Option(1), Option(2), Option(3)).traverse(identity))
  println(List(Option(1), None, Option(3)).traverse(identity))

  // Call .traverse(identity) is the same as call sequence

  println(List(Option(1), Option(2), Option(3)).sequence)
  println(List(Option(1), None, Option(3)).sequence)

  println(List(Option(1), Option(2), Option(3)).sequence_)
  println(List(Option(1), None, Option(3)).sequence_)
}
