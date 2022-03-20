package docs.doc08_identity

import cats._

object IdentityExample extends App {
  // type Id[A] = A
  // Is effect without effect? Context without context?
  val x: Id[Int] = 1
  val y: Int = x

  println(s"[$x == $y] x == y is ${x == y}")

  val one: Int = 1
  val result01 = Functor[Id].map(one)(_ + 1)
  println(result01)

  val result02 = Applicative[Id].pure(73)
  println(result02)

  // Compare the signatures of map and flatMap and coflatMap:
  // def map[A, B](fa: Id[A])(f: A => B): Id[B]
  // def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B]
  // def coflatMap[A, B](a: Id[A])(f: Id[A] => B): Id[B]

  val result03 = Monad[Id].map(one)(_ + 1)
  val result04 = Monad[Id].flatMap(one)(_ + 1)
  println(result03)
  println(result04)

  val fortytwo: Int = 42
  val result05 = Comonad[Id].coflatMap(fortytwo)(_ + 1)
  println(result05)
}
