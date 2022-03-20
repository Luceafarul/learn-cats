package docs.doc04_applicative

import cats.implicits._
import cats.{Applicative, Functor}

import scala.concurrent.Await

object ApplicativeExample extends App {
  // Applicative extends Apply by adding single method pure[A](a: A): F[A]
  val some = Applicative[Option].pure(73)
  val none = Applicative[Option].pure(null)
  val list = Applicative[List].pure(73)

  println(some)
  println(none)
  println(list)

  val listOfSome = Applicative[List] compose Applicative[Option]
  println(listOfSome.pure(73))

  trait CustomApplicative[F[_]] extends Functor[F] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
    def pure[A](a: A): F[A]
  }

  implicit def applicativeForEither[L]: CustomApplicative[Either[L, *]] =
    new CustomApplicative[Either[L, *]] {
      override def product[A, B](
          fa: Either[L, A],
          fb: Either[L, B]
      ): Either[L, (A, B)] = (fa, fb) match {
        case (Right(a), Right(b)) => Right((a, b))
        case (Left(l), _)         => Left(l)
        case (_, Left(l))         => Left(l)
      }

      override def pure[A](a: A): Either[L, A] = Right(a)

      override def map[A, B](fa: Either[L, A])(f: A => B): Either[L, B] =
        fa match {
          case Right(a) => Right(f(a))
          case Left(l)  => Left(l)
        }
    }

  // Applicative compose
  import cats.data.Nested
  import cats.implicits._

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future

  val x: Future[Option[Int]] = Future.successful(Some(5))
  val y: Future[Option[Char]] = Future.successful(Some('a'))

  val compose = Applicative[Future].compose[Option].map2(x, y)(_ + _)

  val nested =
    Applicative[Nested[Future, Option, *]].map2(Nested(x), Nested(y))(_ + _)

  // Wait to result
  import scala.concurrent.duration._
  Await.result(compose, 5.seconds)

  println(compose)
  println(nested)

  // Traverse
  import java.sql.Connection

  val username: Option[String] = Some("username")
  val password: Option[String] = Some("password")
  val url: Option[String] = Some("some.login.url.here")

  // Stub for demonstration purposes
  def attemptConnect(
      username: String,
      password: String,
      url: String
  ): Option[Connection] = None

  def sequenceOption[A](fa: List[Option[A]]): Option[List[A]] =
    traverseOption(fa)(a => a)

//  same as implementation above
//    fa.foldRight(Some(List.empty[A]): Option[List[A]]) { (elem, acc) =>
//      Applicative[Option].map2(elem, acc)(_ :: _)
//    }

  // Alternatively...
  def traverseOption[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(List.empty[B]): Option[List[B]]) { (elem, acc) =>
      val optB = f(elem)
      Applicative[Option].map2(optB, acc)(_ :: _)
    }

  val res01 = traverseOption(List(1, 2, 3, 4, 5))(i => Some(i): Option[Int])
  println(res01)
  val res02 = sequenceOption(List(Some(1), Some(2), Some(3), Some(4), Some(5)))
  println(res02)
  val res03 = sequenceOption(List(Some(1), Some(2), Some(3), Some(4), None))
  println(res03)

  // Syntax
  val res1 = Applicative[Option].map3(username, password, url)(attemptConnect)
  val res2 = (username, password, url).mapN(attemptConnect)

  println(res1)
  println(res2)
}
