package ch03_functors

import cats.Monad
import cats.Functor
import scala.annotation.tailrec

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Leaf(value)         => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }

  val treeMonad: Monad[Tree] = new Monad[Tree] {
    def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(value)         => f(value)
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
    }

    def pure[A](value: A): Tree[A] = Leaf(value)

    // @tailrec
    def tailRecM[A, B](value: A)(f: A => Tree[Either[A, B]]): Tree[B] =
      flatMap(f(value)) {
        case Left(a)  => tailRecM(a)(f)
        case Right(b) => Leaf(b)
      }
  }

  // Book authors recommned add smart constructors for Branch and Leaf
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)
}
