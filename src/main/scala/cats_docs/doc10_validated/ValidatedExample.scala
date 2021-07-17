package cats_docs.doc10_validated

import cats.{Applicative, Apply, Monad, SemigroupK}
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.data.Validated.{Invalid, Valid}
import cats.kernel.Semigroup

object ValidatedExample extends App {
  case class ConnectionParams(url: String, port: Int)

  trait Read[A] {
    def read(s: String): Option[A]
  }

  object Read {
    def apply[A](implicit A: Read[A]): Read[A] = A

    implicit val stringRead: Read[String] = (s: String) => Some(s)

    implicit val intRead: Read[Int] = (s: String) =>
      if (s.matches("-?[0-9]+")) Some(s.toInt)
      else None
  }

  sealed abstract class ConfigError
  final case class MissingConfig(field: String) extends ConfigError
  final case class ParseError(field: String) extends ConfigError

  case class Config(map: Map[String, String]) {
    def parse[A: Read](key: String): Validated[ConfigError, A] = {
      map.get(key) match {
        case Some(value) =>
          Read[A].read(value) match {
            case Some(a) => Valid(a)
            case None    => Invalid(ParseError(key))
          }
        case None => Invalid(MissingConfig(key))
      }
    }
  }

  def parallelValidated[E: Semigroup, A, B, C](
      v1: Validated[E, A],
      v2: Validated[E, B]
  )(f: (A, B) => C): Validated[E, C] =
    (v1, v2) match {
      case (Valid(a), Valid(b))       => Valid(f(a, b))
      case (Valid(_), i @ Invalid(_)) => i
      case (i @ Invalid(_), Valid(_)) => i
      // what should be put here???
      // If we put Invalid(e1, e2) that has type Invalid[(E, E)]
      // But we should return Validate[E, C]
      // How to combine two errors into one error?
      // We can use Semigroup with combine method, like this
      // def parallelValidated[E: Semigroup ...] ...
      // Invalid(Semigroup[E].combine(e1, e2))
      case (Invalid(e1), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))
    }

  implicit val nelSemigroup: Semigroup[NonEmptyList[ConfigError]] =
    SemigroupK[NonEmptyList].algebra[ConfigError]
  implicit val readString: Read[String] = Read.stringRead
  implicit val readInt: Read[Int] = Read.intRead

  val config = Config(Map("url" -> "127.0.0.1", "port" -> "1337"))

  val valid = parallelValidated(
    config.parse[String]("url").toValidatedNel,
    config.parse[Int]("port").toValidatedNel
  )(ConnectionParams)

  println(valid.isValid)
  println(valid.getOrElse(ConnectionParams("", 0)))
  val invalidConfig = Config(
    Map(("endpoint", "127.0.0.1"), ("port", "not a number"))
  )
  val invalid = parallelValidated(
    invalidConfig.parse[String]("url").toValidatedNel,
    invalidConfig.parse[Int]("port").toValidatedNel
  )(ConnectionParams)

  println(invalid.isValid)
  println(invalid)

  implicit def validateApplicative[E: Semigroup]: Applicative[Validated[E, *]] =
    new Applicative[Validated[E, *]] {
      override def pure[A](x: A): Validated[E, A] = Validated.valid(x)

      override def ap[A, B](
          ff: Validated[E, A => B]
      )(fa: Validated[E, A]): Validated[E, B] =
        (fa, ff) match {
          case (Valid(a), Valid(fab))     => Valid(fab(a))
          case (i @ Invalid(_), Valid(_)) => i
          case (Valid(_), i @ Invalid(_)) => i
          case (Invalid(e1), Invalid(e2)) =>
            Invalid(Semigroup[E].combine(e1, e2))
        }
    }

  val personInvalidConfig =
    Config(
      Map(
        ("name", "cat"),
        ("age", "not a number"),
        ("houseNumber", "1234"),
        ("lane", "feline street")
      )
    )

  val personValidConfig =
    Config(
      Map(
        ("name", "Andrew"),
        ("age", "27"),
        ("houseNumber", "1234"),
        ("street", "main street")
      )
    )

  final case class Address(houseNumber: Int, street: String)
  final case class Person(name: String, age: Int, address: Address)

  def personFromConfig(config: Config): ValidatedNel[ConfigError, Person] = {
    Apply[ValidatedNel[ConfigError, *]].map4(
      config.parse[String]("name").toValidatedNel,
      config.parse[Int]("age").toValidatedNel,
      config.parse[Int]("houseNumber").toValidatedNel,
      config.parse[String]("street").toValidatedNel
    ) {
      case (name, age, houseNumber, street) =>
        Person(name, age, Address(houseNumber, street))
    }
  }

  val personFromInvalidConfig: ValidatedNel[ConfigError, Person] =
    personFromConfig(personInvalidConfig)

  print(s"$personFromInvalidConfig and via leftMap: ")
  personFromInvalidConfig.leftMap(e => println(e))

  val personFromValidConfig: ValidatedNel[ConfigError, Person] =
    personFromConfig(personValidConfig)

  print(s"$personFromValidConfig and via foreach: ")
  personFromValidConfig.foreach(e => println(e))

  // flatMap and Either
  implicit def validatedMonad[E]: Monad[Validated[E, *]] =
    new Monad[Validated[E, *]] {
      override def flatMap[A, B](
          fa: Validated[E, A]
      )(f: A => Validated[E, B]): Validated[E, B] =
        fa match {
          case Valid(value)   => f(value)
          case i @ Invalid(_) => i
        }

      override def tailRecM[A, B](a: A)(
          f: A => Validated[E, Either[A, B]]
      ): Validated[E, B] = f(a) match {
        case Valid(a) =>
          a match {
            case Left(value)  => tailRecM(value)(f)
            case Right(value) => Valid(value)
          }
        case i @ Invalid(_) => i
      }

      override def pure[A](x: A): Validated[E, A] = Valid(x)
    }

  val v1 = validatedMonad.tuple2(
    Validated.invalidNel[String, Int]("oops"),
    Validated.invalidNel[String, Double]("uh oh")
  )
  println(v1)

  // Sequential Validation
  // andThen
  def validatedHouseNumber(houseConfig: Config): Validated[ConfigError, Int] =
    houseConfig.parse[Int]("house_number").andThen { number =>
      if (number >= 0) Validated.valid(number)
      else Validated.invalid(ParseError("house_number"))
    }

  val notExistHouseNumber = validatedHouseNumber(personValidConfig)
  println(notExistHouseNumber)

  val houseNumberConfig = Config(Map("house_number" -> "-42"))
  val houseNumber = validatedHouseNumber(houseNumberConfig)
  println(houseNumber)

  // withEither
  import cats.implicits._

  def positive(field: String, i: Int): Either[ConfigError, Int] = {
    if (i >= 0) Either.right(i)
    else Either.left(ParseError(field))
  }

  val houseNumberEither = houseNumberConfig.parse[Int]("house_number").withEither {
    either: Either[ConfigError, Int] => either.flatMap { i =>
      println(positive("house_number", i))
      positive("house_number", i)
    }
  }
  println(houseNumberEither)
}
