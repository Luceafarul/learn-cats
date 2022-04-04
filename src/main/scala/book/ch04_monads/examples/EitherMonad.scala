package book.ch04_monads.examples

import scala.util.Try

object EitherMonad extends App {

  import cats.implicits._

  val a = 3.asRight[String]
  val b = 7.asRight[String]

  val ab = for {
    x <- a
    y <- b
  } yield x * y

  println(ab)

  //  This example will not compile:
  //  def countPositive(xs: List[Int]): Either[String, Int] =
  //    xs.foldLeft(Right(0)) { (acc, x) =>
  //      if (x > 0) acc.map(_ + 1) else Left("Negative. Stopping!")
  //    }

  def countPositive1(xs: List[Int]): Either[String, Int] =
    xs.foldLeft(Right(0): Either[String, Int]) { (acc, x) =>
      if (x > 0) acc.map(_ + 1) else Left("Negative. Stopping!")
    }

  def countPositive2(xs: List[Int]): Either[String, Int] =
    xs.foldLeft(0.asRight[String]) { (acc, x) =>
      if (x > 0) acc.map(_ + 1) else Left("Negative. Stopping!")
    }

  println(countPositive1(List(1, 2, 3)))
  println(countPositive2(List(1, -2, 3)))

  println(Either.catchOnly[NumberFormatException]("foo".toInt))
  println(Either.catchOnly[NumberFormatException]("123".toInt))

  println(Either.catchNonFatal("foo".toInt))
  println(Either.catchNonFatal("123".toInt))
  println(Either.catchNonFatal(sys.error("Badness")))

  println(Either.fromTry(Try("foo".toInt)))
  println(Either.fromTry(Try("123".toInt)))

  println(Either.fromOption(None, "Badness"))
  println(Either.fromOption(Some(123), "Badness"))

  println("Error".asLeft[Int].getOrElse(0))
  println("Error".asLeft[Int].orElse(2.asRight[String]))

  println(-1.asRight[String].ensure("Must be non-negative!")(_ > 0))

  println("Error".asLeft[Int].recover { case _: String => -1 })
  println("Error".asLeft[Int].recoverWith { case _: String => Right(-1) })

  println("Error".asLeft[Int].leftMap(_.reverse))

  println(7.asRight[String].bimap(_.reverse, _ * 7))
  println("Error".asLeft[Int].bimap(_.reverse, _ * 7))

  println(123.asRight[String])
  println(123.asRight[String].swap)

  val result = for {
    a <- 1.asRight[String]
    b <- 0.asRight[String]
    c <- if (b == 0) "DIV BY 0".asLeft[Int] else (a / b).asRight[String]
  } yield c * 100
  println(result)

  object wrapper {
    sealed trait LoginError extends Product with Serializable
    final case class UserNotFound(username: String) extends LoginError
    final case class PasswordIncorrect(username: String) extends LoginError
    case object UnexpectedError extends LoginError
  }

  import wrapper._

  final case class User(username: String, password: String)

  type LoginResult = Either[LoginError, User]

  def handleError(error: LoginError): Unit =
    error match {
      case UserNotFound(username) => println(s"User: $username not found")
      case PasswordIncorrect(username) => println(s"Incorrect password for user: $username")
      case wrapper.UnexpectedError => println("Unexpected error")
    }

  val successLogin: LoginResult = User("test", "12345").asRight
  val failedLogin: LoginResult = UserNotFound("dave").asLeft

  successLogin.fold(handleError, println)
  failedLogin.fold(handleError, println)
}
