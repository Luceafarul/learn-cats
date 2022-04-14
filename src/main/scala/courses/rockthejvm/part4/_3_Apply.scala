package courses.rockthejvm.part4

import cats.{Functor, Semigroupal}

object _3_Apply extends App {
  // Apply is a weaker Applicative
  trait _Apply[F[_]] extends Functor[F] with Semigroupal[F] {
    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] // Fundamental

    def mapN[A, B, C](tuple: (F[A], F[B]))(f: (A, B) => C): F[C] = {
      val (fa, fb) = tuple
      ap(map(fa)(a => (b: B) => f(a, b)))(fb)
    }

    def mapNSolution[A, B, C](tuple: (F[A], F[B]))(f: (A, B) => C): F[C] = {
      val (fa, fb) = tuple
      val tupled = product(fa, fb)
      map(tupled) { case (a, b) => f(a, b) }
    }

    override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
      val functionWrapper: F[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
      ap(functionWrapper)(fb)
    }
  }

  trait _Applicative[F[_]] extends _Apply[F] {
    def pure[A](a: A): F[A] // Fundamental
  }

  import cats.Apply

  val applyOption = Apply[Option]
  val funcApp = applyOption.ap(Some((x: Int) => x * 2))(Some(3))
  println(funcApp)

  import cats.syntax.apply._

  val tupleOfOptions = (Option(1), Option(2), Option(3))
  val optionOfTuple = tupleOfOptions.tupled
  println(s"Option of tuple: $optionOfTuple")

  val sumOption = tupleOfOptions.mapN(_ + _ + _)
  println(s"Sum of Option: $sumOption\n")

  println(s"ProductR with Some and Some: ${Option(true) *> Option(7)}")
  println(s"ProductR with None and Some: ${None *> Option(5)}")
  println(s"ProductR with Some and None: ${Option(true) *> None}\n")

  println(s"ProductL with Some and Some: ${Option(true) <* Some(13)}")
  println(s"ProductL with None and Some: ${None <* Some(13)}")
  println(s"ProductL with Some and None: ${Option(true) <* None}")
}
