package book.ch04_monads

object Id {
  import cats.Id

  def pure[A](a: A): Id[A] = a

  def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)

  def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
}
