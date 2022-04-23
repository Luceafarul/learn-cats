package courses.rockthejvm.part4

import cats.{Applicative, Foldable, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object _7_Traversing extends App {

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val servers: List[String] = List(
    "green-ci.rockthejvm.com",
    "green-stage.rockthejvm.com",
    "prod.rockthejvm.com",
  )

  def bandwidth(hostname: String): Future[Int] = Future(hostname.length * 10)

  // We have:
  // - a List[String]
  // - a func String => Future[Int]
  // We want a Future[List[Int]]
  val allBandwidth: Future[List[Int]] =
    servers.foldLeft(Future(List.empty[Int])) { (acc, hostname) =>
      val bandFuture: Future[Int] = bandwidth(hostname)
      for {
        bandwidths <- acc
        band <- bandFuture
      } yield bandwidths :+ band
    }

  val allBandwidthsTraverse: Future[List[Int]] = Future.traverse(servers)(bandwidth)
  val allBandwidthsSequence: Future[List[Int]] = Future.sequence(servers.map(bandwidth))

  allBandwidthsTraverse.onComplete(println)
  allBandwidthsSequence.onComplete(println)

  // Exercise:
  def listTraverse[F[_]: Monad, A, B](xs: List[A])(f: A => F[B]): F[List[B]] =
    xs.foldLeft(Monad[F].pure(List.empty[B])) { (acc, elem) =>
      Monad[F].flatMap(acc)(a => Monad[F].map(f(elem))(b => a :+ b))
    }

  import cats.syntax.applicative._ // pure
  import cats.syntax.flatMap._ // flatMap
  import cats.syntax.functor._ // map
  def solution1[F[_]: Monad, A, B](xs: List[A])(f: A => F[B]): F[List[B]] =
    xs.foldLeft(List.empty[B].pure[F]) { (acc, elem) =>
      for {
        a <- acc
        e <- f(elem)
      } yield a :+ e
    }

  import cats.syntax.apply._ // mapN
  def solution2[F[_]: Applicative, A, B](xs: List[A])(f: A => F[B]): F[List[B]] =
    xs.foldLeft(List.empty[B].pure[F]) { (acc, e) => (acc, f(e)).mapN(_ :+ _) }

  // Exercise:
  def listSequence[F[_]: Applicative, A](xs: List[F[A]]): F[List[A]] =
    xs.foldLeft(List.empty[A].pure[F])((acc, e) => (acc, e).mapN(_ :+ _))

  def solution3[F[_]: Applicative, A](xs: List[F[A]]): F[List[A]] =
    solution2(xs)(identity)

  // Exercise:
  println(listSequence(List(Vector(1, 2), Vector(3, 4)))) // Vector(List(1, 2), List(3, 4))
  println(listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))) // Vector(List(1, 2), List(3, 4), List(4, 5))

  // Wrong. Correct answer - all the possible tuples

  def filterAsOption(xs: List[Int])(p: Int => Boolean): Option[List[Int]] =
    listTraverse[Option, Int, Int](xs)(n => Some(n).filter(p))

  // Exercise: what's the result of. Result: eq to forAll method :)
  println(filterAsOption(List(2, 4, 6))(_ % 2 == 0)) // Some(List(2, 4, 5))
  println(filterAsOption(List(2, 4, 6))(_ % 3 == 0)) // Some(List(6)) -> Wrong here None too
  println(filterAsOption(List(2, 4, 7))(_ % 3 == 0)) // None

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]

  // If using solution2 (with Applicative) errors was appended,
  // but if using listTraverse (with Monad) was collect only first error.
  def filterAsValidated(xs: List[Int])(p: Int => Boolean): ErrorsOr[List[Int]] =
    solution2[ErrorsOr, Int, Int](xs) { n =>
      if (p(n)) Validated.valid(n)
      else Validated.invalid(List(s"Predicate for $n failed."))
    }

  // Exercise: what's the result of. Result: looks like failed on first?
  println(filterAsValidated(List(2, 4, 6))(_ % 2 == 0)) // Valid(List(2, 4, 6))
  println(filterAsValidated(List(2, 4, 6))(_ % 3 == 0)) // Invalid for 2 and 4
  println(filterAsValidated(List(2, 4, 7))(_ % 3 == 0)) // Invalid for all

  trait _Traverse[F[_]] extends Foldable[F] {
    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

    def sequence[G[_]: Applicative, A](ga: F[G[A]]): G[F[A]] =
      traverse(ga)(identity)

    type Identity[T] = T
    def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Identity, A, B](fa)(f)
  }

  import cats.Traverse
  val allBandwidthCats: Future[List[Int]] = Traverse[List].traverse(servers)(bandwidth)

  allBandwidthCats.onComplete(println)

  import cats.syntax.traverse._
  servers.traverse(bandwidth).onComplete(println)
}
