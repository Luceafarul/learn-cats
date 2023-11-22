package blogs.free_monad

import cats.free.Free
import cats.kernel.Monoid
import cats.arrow.FunctionK
import cats.{~>, Id}
import blogs.free_monad.FreeMonadDPRecursive.Store.Put
import blogs.free_monad.FreeMonadDPRecursive.Store.Get

// Example of https://levelup.gitconnected.com/functional-dynamic-programming-scala-cats-and-free-monad-b71c34c209
object FreeMonadDPRecursive extends App {
  // Step 1: Define the algebra
  sealed trait Store[A] extends Serializable with Product

  object Store {
    final case class Put[T](index: Int, value: T) extends Store[Unit]
    final case class Get[T](index: Int) extends Store[Option[T]]
  }

  // Step 2: Creating the DSL (Domain Specific Language), using smart constructors
  type ArrayStore[A] = Free[Store, A]

  def put[T](index: Int, value: T): ArrayStore[Unit] =
    Free.liftF[Store, Unit](Store.Put(index, value))

  def get[T](index: Int): ArrayStore[Option[T]] =
    Free.liftF[Store, Option[T]](Store.Get(index))

  // Step 3: Build program using DSL
  def cdRecursive(i: Int): ArrayStore[Option[BigInt]] =
    for {
      v <- get[BigInt](i)
      iMinusOne <- if (v == Option(-1)) cdRecursive(i - 1) else get[BigInt](2)
      iMinusTwo <- if (v == Option(-1)) cdRecursive(i - 2) else get[BigInt](1)
      _ <-
        if (v == Option(BigInt(-1)))
          put[BigInt](i, (i - 1) * (Monoid[Option[BigInt]].combine(iMinusOne, iMinusTwo).get))
        else put[BigInt](1, 0)
      newV <- get[BigInt](i)
    } yield newV

  // Step 4: Interpreter for executing our program description to execute it.
  def interpreter(n: Int): Store ~> Id = new (Store ~> Id) {
    val array = new Array[Any](n + 1)
    array(1) = BigInt(0)
    array(2) = BigInt(1)
    3.to(n).foreach(i => array(i) = -1)

    def apply[A](fa: Store[A]): Id[A] =
      fa match {
        case Put(index, value) =>
          println(s"put($index, $value)")
          array(index) = value
          ()
        case Get(index) =>
          println(s"--- get($index) ---")
          Option(array(index).asInstanceOf[A])
      }
  }

  // Step 5: In the end to execute our program, just foldMap the program using interpreter,
  // and supply “n”, where n is just the integer we are passing to compute derangements
  // for n items and also the interpreter will use n to create array of n for memoization.

  // Free.foldMap executes in a stack-safe way using trampolining technique.

  val result = cdRecursive(10).foldMap(interpreter(10))

  println(s"Result: $result")
}
