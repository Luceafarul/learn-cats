package courses.rockthejvm.part2

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object _4_Monads extends App {
  // Lists
  val numbersList = List(1, 2, 3)
  val charsList = List('a', 'b', 'c')

  // How to create all combinations of (number, char)
  val result01 = numbersList.flatMap(n => charsList.map((n, _)))
  val result02 = for {
    n <- numbersList
    c <- charsList
  } yield (n, c)

  println(result01)
  println(result02)

  // Options
  val numberOption = Option(3)
  val charOption = Option('c')

  // How to create all combinations of (number, char)
  val result03 = numberOption.flatMap(n => charOption.map(c => (n, c)))
  val result04 = for {
    n <- numberOption
    c <- charOption
  } yield (n, c)

  println(result03)
  println(result04)

  // Futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
  val numberFuture = Future(3)
  val charFuture = Future('c')

  // How to create all combinations of (number, char)
  val result05 = numberFuture.flatMap(n => charFuture.map(c => (n, c)))
  val result06 = for {
    n <- numberFuture
    c <- charFuture
  } yield (n, c)

  result05.onComplete(println)
  result06.onComplete(println)

  // Pattern:
  // - wrapping a value into a M (monadic) value
  // - the flatMap mechanism
  //
  // MONADS
  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(a => pure(f(a)))
  }

  // Cats Monad
  import cats.Monad
  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(3)
  val aTransformedOption = optionMonad.flatMap(anOption)(x => if (x % 2 == 0) Some(x + 1) else None)

  println(anOption)
  println(aTransformedOption)

  val listMonad = Monad[List]
  val aList = listMonad.pure(3)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1, x + 2))

  println(aList)
  println(aTransformedList)

  val futureMonad = Monad[Future]
  val aFuture = futureMonad.pure(13)
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x => Future(x * 10))

  aFuture.onComplete(println)
  aTransformedFuture.onComplete(println)

  // Specialized API
  def pairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] =
    for {
      n <- numbers
      c <- chars
    } yield (n, c)

  def pairsOption(numbers: Option[Int], chars: Option[Char]): Option[(Int, Char)] =
    for {
      n <- numbers
      c <- chars
    } yield (n, c)

  def pairsFuture(numbers: Future[Int], chars: Future[Char]): Future[(Int, Char)] =
    for {
      n <- numbers
      c <- chars
    } yield (n, c)

  // General API
  import cats.implicits._
  def pairs[F[_] : Monad, A, B](xs: F[A], ys: F[B]): F[(A, B)] =
    for {
      x <- xs
      y <- ys
    } yield (x, y)

  def pairs2[F[_] : Monad, A, B](xs: F[A], ys: F[B]): F[(A, B)] =
    xs.flatMap(x => ys.flatMap(y => (x, y).pure[F]))

  println(pairs(numbersList, charsList))
  println(pairs2(numbersList, charsList))

  println(pairs(numberOption, charOption))
  println(pairs2(numberOption, charOption))

  // Extension methods:
  val oneOption = 1.pure[Option]
  val oneList = 1.pure[List]

  println(oneOption)
  println(oneList)

  val composerOptionFor = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two
  println(composerOptionFor)
}
