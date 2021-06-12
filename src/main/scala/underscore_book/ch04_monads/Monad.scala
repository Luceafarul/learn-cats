package underscore_book.ch04_monads

trait Monad[F[_]] {
  def pure[A](value: A): F[A]

  def flatMap[A, B](value: F[A])(f: A => F[B]): F[B]

  def map[A, B](value: F[A])(f: A => B): F[B] = flatMap(value)(a => pure(f(a)))
}
