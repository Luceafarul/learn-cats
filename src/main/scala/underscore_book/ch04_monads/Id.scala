package underscore_book.ch04_monads

import cats.Id

object Id {
  def pure[A](value: A): Id[A] = value

  def map[A, B](initial: Id[A])(f: A => B): Id[B] = f(initial)
  
  def flatMap[A, B](initial: Id[A])(f: A => Id[B]): Id[B] = f(initial)
}
