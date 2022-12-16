package book.ch05_monad_transformers.examples

import cats.Monad
import cats.syntax.applicative._

import scala.util.{Success, Try}

object Compose extends App {
  // Hypothetical example. This won't actually compile:
  def compose[M1[_] : Monad, M2[_] : Monad] = {
    type Composed[A] = M1[M2[A]]
    new Monad[Composed] {
      def pure[A](a: A): Composed[A] = a.pure[M2].pure[M1]

      def flatMap[A, B](fa: Composed[A])(f: A => Composed[B]): Composed[B] =
        ???
      // Problem! How do we write flatMap?
      // We need to know extra knowledge about how to compose that monad with others

      override def tailRecM[A, B](a: A)(f: A => Composed[Either[A, B]]): Composed[B] = ???
    }
  }

  import cats.data.OptionT

  type ListOption[A] = OptionT[List, A]

  import cats.instances.list._
  import cats.syntax.applicative._

  val result1: ListOption[Int] = OptionT(List(Option(10)))
  val result2: ListOption[Int] = 32.pure[ListOption]

  val sumResult = result1.flatMap { x =>
    result2.map { y =>
      x + y
    }
  }
  println(sumResult)
  println(sumResult.value)

  type ErrorOr[A] = Either[String, A]
  type ErrorOrOption[A] = OptionT[ErrorOr, A]

  val ten = 10.pure[ErrorOrOption]
  val eleven = 11.pure[ErrorOrOption]

  val twentyOne = ten.flatMap(x => eleven.map(_ + x))

  println(twentyOne)
  println(twentyOne.value)

  import scala.concurrent.Future
  import cats.data.EitherT

  type FutureEither[A] = EitherT[Future, String, A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]

  import scala.concurrent.ExecutionContext.Implicits.global

  val futureEitherOr: FutureEitherOption[Int] =
    for {
      a <- 40.pure[FutureEitherOption]
      b <- 37.pure[FutureEitherOption]
    } yield a + b

  futureEitherOr.value.value.onComplete(println)

  import cats.data.Writer

  type Logged[A] = Writer[List[String], A]

  // Methods generally return untransformed stacks:
  def parseNumber(s: String): Logged[Option[Int]] =
    Try(s.toInt).toOption match {
      case Some(num) => Writer(List(s"Read $s"), Some(num))
      case None => Writer(List(s"Failed on $s"), None)
    }

  // Consumers use monad transformers locally to simplify composition:
  def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
    import cats.data.OptionT
    val result = for {
      a <- OptionT(parseNumber(a))
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    } yield a + b + c
    result.value
  }

  val resultOne = addAll("1", "2", "3")
  val resultTwo = addAll("1", "a", "3")

  println(resultOne)
  println(resultOne.value)
  println(resultTwo)
  println(resultTwo.value)

  // TODO: how to create addAll that got List[String] as input
}
