package ch03_functors

import cats.Functor

final case class Box[A](value: A)

object Box {
  implicit val boxFunctor: Functor[Box] = new Functor[Box] {
    def map[A, B](box: Box[A])(f: A => B): Box[B] = Box(f(box.value))
  }
}

