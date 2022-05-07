package book.ch04_monads.exercises

import cats.Monad

import scala.annotation.tailrec

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    def pure[A](x: A): Tree[A] = leaf(x)

    def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
      fa match {
        case Branch(left, right) => branch(flatMap(left)(f), flatMap(right)(f))
        case Leaf(value)         => f(value)
      }

    // Not tailrec implementation
    def tailRecMSimple[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
      flatMap(f(a)) {
        case Left(a)  => tailRecM(a)(f)
        case Right(b) => leaf(b)
      }

    // Tailrec implementation
    def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      @tailrec
      def loop(open: List[Tree[Either[A, B]]], closed: List[Option[Tree[B]]]): List[Tree[B]] =
        open match {
          case Branch(left, right) :: next => loop(left :: right :: next, None :: closed)
          case Leaf(Left(a)) :: next       => loop(f(a) :: next, closed)
          case Leaf(Right(b)) :: next      => loop(next, Some(pure(b)) :: closed)
          case Nil =>
            closed.foldLeft(List.empty[Tree[B]]) { (acc, maybeTree) =>
              maybeTree.map(_ :: acc).getOrElse {
                val left :: right :: tail = acc
                branch(left, right) :: tail
              }
            }
        }

      loop(List(f(a)), Nil).head
    }
  }
}
