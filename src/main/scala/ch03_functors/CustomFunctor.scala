package ch03_functors

import cats.Functor
import ch01_intro.p02_exercise._

final case class Box[A](value: A)

object Box {
  implicit val boxFunctor: Functor[Box] = new Functor[Box] {
    def map[A, B](box: Box[A])(f: A => B): Box[B] = Box(f(box.value))
  }

  implicit def printableBox[A](implicit printable: Printable[A]): Printable[Box[A]] = (box: Box[A]) => s"Box[${printable.format(box.value)}]"

//   implicit def printableBox(box: Int)(implicit printable: Printable[Int]): Printable[Int] = printable.contramap(box)
}
