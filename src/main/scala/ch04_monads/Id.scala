package ch04_monads

import ch04_monads.Monad

// TODO:
//  1. How to extends Monad?
//  2. How to remove constructor param?
case class Id[A](value: A) { // extends Monad[Id[A]] { ---> get error :(
  import Id._

  def flatMap[A, B](id: Id[A])(f: A => Id[B]): Id[B] = f(id.value)

  def map[A, B](id: Id[A])(f: A => B): Id[B] = flatMap(id)(a => pure(f(a)))
}

object Id {
  def pure[A](value: A): Id[A] = Id(value)
}
