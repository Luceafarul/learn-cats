package courses.rockthejvm.part2

import scala.annotation.tailrec

object _6_CustomMonads extends App {

  import cats.Monad

  implicit object OptionMonad extends Monad[Option] {
    def pure[A](x: A): Option[A] = Option(x)

    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    @tailrec
    def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] =
      f(a) match {
        case None => None
        case Some(Left(a)) => tailRecM(a)(f)
        case Some(Right(b)) => Some(b)
      }
  }

  // Define a monad for the identity type
  type Identity[T] = T

  implicit object IdentityMonad extends Monad[Identity] {
    def pure[A](x: A): Identity[A] = x

    def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)

    @tailrec
    def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] =
      f(a) match {
        case Left(a) => tailRecM(a)(f)
        case Right(b) => b
      }
  }

  val aNumber: Identity[Int] = 73
  println(aNumber)

  // Define monad for Tree
  sealed trait Tree[+A]

  final case class Leaf[+A](value: A) extends Tree[A]

  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  implicit object TreeMonad extends Monad[Tree] {
    def pure[A](x: A): Tree[A] = Leaf(x)

    def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
      fa match {
        case Leaf(value) => f(value)
        case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
      }

    def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def loop(tree: Tree[Either[A, B]]): Tree[B] = {
        tree match {
          case Leaf(Left(a)) => loop(f(a))
          case Leaf(Right(b)) => Leaf(b)
          case Branch(left, right) => Branch(loop(left), loop(right))
        }
      }

      @tailrec
      def tailRec(todo: List[Tree[Either[A, B]]], expanded: Set[Tree[Either[A, B]]], done: List[Tree[B]]): Tree[B] =
        if (todo.isEmpty) done.head
        else todo.head match {
          case Leaf(Left(a)) => tailRec(f(a) :: todo.tail, expanded, done)
          case Leaf(Right(b)) => tailRec(todo.tail, expanded, Leaf(b) :: done)
          case branch @ Branch(left, right) =>
            if (!expanded.contains(branch)) tailRec(right :: left :: todo, expanded + branch, done)
            else {
              val newLeft = done.head
              val newRight = done.tail.head
              val newBranch = Branch(newLeft, newRight)
              tailRec(todo.tail, expanded, newBranch :: done.drop(2))
            }
        }

      tailRec(List(f(a)), Set.empty, List.empty)
    }
  }

  val tree: Tree[Int] = Branch(
    Branch(Leaf(1), Leaf(2)),
    Leaf(3)
  )
  val changedTree = TreeMonad.flatMap(tree)(t => Branch(Leaf(t * 2), Leaf(t * 3)))
  println(changedTree)
}
