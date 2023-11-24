package blogs.monad

import scala.util.Try

// https://blog.rockthejvm.com/another-take-on-monads/
object AnotherTakeAtMonad extends App {
  // The problem:
  // We need combine String and Number that can be wrapped in some context (structure)
  // List, Option, Try

  def combineList(ss: List[String])(ns: List[Int]): List[(String, Int)] =
    for {
      s <- ss
      n <- ns
    } yield s -> n

  def combineOption(ss: Option[String])(ns: Option[Int]): Option[(String, Int)] =
    for {
      s <- ss
      n <- ns
    } yield s -> n

  def combineTry(ss: Try[String])(ns: Try[Int]): Try[(String, Int)] =
    for {
      s <- ss
      n <- ns
    } yield s -> n

  // As we can see these method have the same implementation, let's try to generalize it

  // Create a type class Monad, that provide
  // - creating an instance of this magical data type (whatever the type is) out of a plain value
  // - transforming an instance to another type of instance through a function, i.e. a map
  // - chaining the computation of instances based on dependent plain values, i.e. a flatMap
  trait Monad[F[_]] {
    def pure[A](a: A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))
  }

  // Now we can rewrite our method
  def combine[F[_]](ss: F[String])(ns: F[Int])(implicit monad: Monad[F]): F[(String, Int)] =
    monad.flatMap(ss)(s => monad.map(ns)(n => s -> n))

  // Ohay, but if we write
  // println(combine(List("a", "b", "c"))(List(1, 2, 3)))

  // We get an error:
  // could not find implicit value for parameter monad: blogs.monad.AnotherTakeAtMonad.Monad[List]

  // So, we need provide implicit parameter of Monad[List], let's create it:
  implicit val monadList: Monad[List] = new Monad[List] {
    def pure[A](a: A): List[A] = List(a)
    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
  }

  // And now our method working
  println(combine(List("a", "b", "c"))(List(1, 2, 3)))

  // Let's do the same for Try
  implicit val monadTry: Monad[Try] = new Monad[Try] {
    def pure[A](a: A): Try[A] = Try(a)
    def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)
  }

  println(combine(Try("a"))(Try(1)))

  // Ohay, let's rewrite our combine method using for-comprehension like our first 3 method
  def combineWithFor[F[_]](ss: F[String])(ns: F[Int])(implicit monad: Monad[F]): F[(String, Int)] =
    for {
      s <- ss
      n <- ns
    } yield s -> n

  // Omg, what is that? Errors?
  // value flatMap is not a member of type parameter F[String]
  // value map is not a member of type parameter F[Int]

  // But why?
  // We need add syntas for that (Scala 2) or extension method (Scala 3)

  implicit class MonadOps[F[_]: Monad, A](fa: F[A]) { //(implicit F: Monad[F]) {
    private val F = implicitly[Monad[F]]
    def map[B](f: A => B): F[B] = F.map(fa)(f)
    def flatMap[B](f: A => F[B]): F[B] = F.flatMap(fa)(f)
  }

  println(combineWithFor(Try("a"))(Try(1)))
}
