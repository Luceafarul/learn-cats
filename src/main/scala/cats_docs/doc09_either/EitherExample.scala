package cats_docs.doc09_either

import cats.implicits._

object EitherExample extends App {
  // Either vs Validate
  // In general, Validated is used to accumulate errors,
  // while Either is used to short-circuit a computation upon the first error.
  object ExceptionStyle {
    def parse(s: String): Int =
      if (s.matches("-?[0-9]+")) s.toInt
      else throw new NumberFormatException(s"$s is not a valid integer.")

    def reciprocal(i: Int): Double =
      if (i == 0) throw new IllegalArgumentException("Cannot take reciprocal of 0.")
      else 1.0 / i

    def stringify(d: Double): String = d.toString
  }

  object EitherStyle {
    def parse(s: String): Either[NumberFormatException, Int] =
      if (s.matches("-?[0-9]+")) Right(s.toInt)
      else Left(new NumberFormatException(s"$s is not a valid integer."))

    def reciprocal(i: Int): Either[IllegalArgumentException, Double] =
      if (i == 0) Left(new IllegalArgumentException("Cannot take reciprocal of 0."))
      else Right(1.0 / i)

    def stringify(d: Double): String = d.toString

    def magic(s: String): Either[Exception, String] =
      parse(s).flatMap(reciprocal).map(stringify)
  }

  import EitherStyle._
  val result01 = magic("2") match {
    case Left(_: NumberFormatException) => "Not a number!"
    case Left(_: IllegalArgumentException) => "Can't take reciprocal of 0!"
    case Left(_) => "Unknown error"
    case Right(value) => s"Got reciprocal: $value"
  }

  println(result01)

  object EitherStyleWithAdts {
    sealed trait Error
    final case class NotANumber(message: String) extends Error
    final case object NoZeroReciprocal extends Error

    def parse(s: String): Either[Error, Int] =
      if (s.matches("-?[0-9]+")) Either.right(s.toInt)
      else Either.left(NotANumber(s"$s is not a valid integer."))

    def reciprocal(i: Int): Either[Error, Double] =
      if (i == 0) Either.left(NoZeroReciprocal)
      else Either.right(1.0 / i)

    def stringify(d: Double): String = d.toString

    def magic(s: String): Either[Error, String] =
      parse(s).flatMap(reciprocal).map(stringify)
  }

  // Our custom error hierarchy make matching much nicer
  import EitherStyleWithAdts._
  import EitherStyleWithAdts.{magic => magicWithAdts}

  val result02 = magicWithAdts("4") match {
    case Left(NotANumber(message)) => s"$message is not a number!"
    case Left(NoZeroReciprocal) => "Can't take reciprocal of 0!"
    case Right(value) => s"Got reciprocal: $value"
  }

  println(result02)

  // Either in the small, Either in the large
  // What we can do if we have two separate modules which give back separate kinds of errors
  sealed abstract class DatabaseError
  trait DatabaseValue

  sealed abstract class ServiceError
  trait ServiceValue

  // Solution 1: Application-wide errors
  sealed abstract class AppError
  final case object DatabaseError1 extends AppError
  final case object DatabaseError2 extends AppError
  final case object ServiceError1 extends AppError
  final case object ServiceError2 extends AppError

  // Solution 2: ADTs all the way down
  sealed abstract class DatabaseError1
  trait DatabaseValue1

  sealed abstract class ServiceError1
  trait ServiceValue1
  sealed abstract class AppError1
  object AppError1 {
    final case class Database(error: DatabaseError1) extends AppError
    final case class Service(error: ServiceError1) extends AppError
  }

  // Additional syntax
  val right: Either[String, Int] = 73.asRight[String]
  val left: Either[String, Int] = "hello 🐈s".asLeft[Int]

  println(right)
  println(left)
}
