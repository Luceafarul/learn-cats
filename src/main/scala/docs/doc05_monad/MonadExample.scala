package docs.doc05_monad

import cats.implicits.catsStdInstancesForOption

import scala.annotation.tailrec

object MonadExample extends App {
  println("flatten examples:")
  println(Option(Option(1)).flatten)
  println(Option(None).flatten)
  println(List(List(1), List(2, 3)).flatten)

  // Monad instances
  println("\npure examples:")
  import cats._
  implicit def optionMonad(implicit app: Applicative[Option]): Monad[Option] =
    new Monad[Option] {
      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
        app.map(fa)(f).flatten
      override def pure[A](x: A): Option[A] = app.pure(x)

      @tailrec
      override def tailRecM[A, B](
          a: A
      )(f: A => Option[Either[A, B]]): Option[B] =
        f(a) match {
          case None           => None
          case Some(Left(a1)) => tailRecM(a1)(f)
          case Some(Right(b)) => Some(b)
        }

    }

  println(Monad[Option].pure(42))

  // FlatMap
  println("\nflatMap examples:")
  implicit val listMonad: Monad[List] = new Monad[List] {
    override def pure[A](x: A): List[A] = List(x)

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => List[Either[A, B]]): List[B] = ???
  }

  println(Monad[List].flatMap(List(1, 2, 3))(x => List(x, x)))

  // ifM
  println("\nifM examples:")
  println(Monad[Option].ifM(Option(true))(Option("Truthy"), Option("Falsy")))
  println(Monad[Option].ifM(Option(false))(Option("Truthy"), Option("Falsy")))
  println(Monad[List].ifM(List(true, false, true))(List(1, 2), List(3, 4)))

  // Composition
  println("\ncomposition examples:")

  final case class OptionT[F[_], A](value: F[Option[A]])
  implicit def optionTMonad[F[_]](implicit F: Monad[F]): Monad[OptionT[F, *]] =
    new Monad[OptionT[F, *]] {
      override def pure[A](x: A): OptionT[F, A] = OptionT(F.pure(Some(x)))

      override def flatMap[A, B](fa: OptionT[F, A])(
        f: A => OptionT[F, B]
      ): OptionT[F, B] = OptionT {
        F.flatMap(fa.value) {
          case None => F.pure(None)
          case Some(a) => f(a).value
        }
      }

      override def tailRecM[A, B](a: A)(
          f: A => OptionT[F, Either[A, B]]
      ): OptionT[F, B] = ???
    }
  // This sort of construction is called a monad transformer.
  // Cats already provides a monad transformer for Option called OptionT.

  println(optionTMonad[List].pure(42))
  println(optionTMonad[List].pure(42).value)
}
