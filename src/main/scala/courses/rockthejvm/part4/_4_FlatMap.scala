package courses.rockthejvm.part4

import cats.{Applicative, Apply, FlatMap}

object _4_FlatMap extends App {
  trait _FlatMap[F[_]] extends Apply[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    // Hint: Apply extends Functor
    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
      flatMap(fa)(a => map(ff)(f => f(a)))
    //         |  |        /    \    \/
    //         |  |    F[A=>B] A=>B  B
    //         |  |     \____   ____/
    //       F[A] A =>      F[B]
  }

  trait _Monad[F[_]] extends Applicative[F] with _FlatMap[F] {
    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      flatMap(fa)(a => pure(f(a)))
  }

  // FlatMap is a weaker Monad :)
  // Monad   extends FlatMap with Applicative
  // FlatMap extends Apply
  // Apply   extends Functor
  // Monad does not have it's own new methods

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def pairs[F[_]: FlatMap](numbers: F[Int], chars: F[Char]): F[(Int, Char)] =
    for {
      n <- numbers
      c <- chars
    } yield (n, c)

  println(s"Pairs: ${pairs(List(1, 2, 3), List('a', 'b'))}")
}
