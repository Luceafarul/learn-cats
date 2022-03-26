package courses.rockthejvm.part2

import scala.util.Try

object _3_Functors extends App {
  // Useful when need to generalize a transformation

  val mappedList = List(1, 2, 3).map(_ + 1)
  val mappedOption = Option(1).map(_ + 1)
  val mappedTry = Try(1).map(_ + 1)

  // Simplified definition
  trait SimplifiedFunctor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  // Cats Functor
  import cats.Functor

  val listFunctor = Functor[List]
  val numbers = List(1, 2, 3, 4, 5)
  val incrementedNumbers = listFunctor.map(numbers)(_ + 1)
  println(incrementedNumbers)

  val incrementedOption = Functor[Option].map(Some(3))(_ + 1)
  println(incrementedOption)

  val incrementedTry = Functor[Try].map(Try(13))(_ + 1)
  println(incrementedTry)

  def do10xList(xs: List[Int]): List[Int] = xs.map(_ * 10)
  def do10xOption(xs: Option[Int]): Option[Int] = xs.map(_ * 10)
  def do10xTry(xs: Try[Int]): Try[Int] = xs.map(_ * 10)

  // Generalize method above
  def do10x[F[_]](fa: F[Int])(implicit functor: Functor[F]): F[Int] =
    functor.map(fa)(_ * 10)

  println(s"Not generalized: ${do10xList(numbers)}")
  println(s"Generalized:     ${do10x(numbers)}")

  // Define functor for a binary tree
  sealed trait Tree[+T]
  final case class Leaf[+T](value: T) extends Tree[T]
  final case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  object Tree {
    def leaf[T](value: T): Tree[T] = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
    implicit object TreeFunctor extends Functor[Tree] {
      def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
        case Leaf(value) => Leaf(f(value))
        case Branch(value, left, right) => Branch(f(value), map(left)(f), map(right)(f))
      }
    }
  }

  val exampleTree = Branch(
    1,
    Branch(
      2,
      Leaf(3),
      Leaf(4)
    ),
    Leaf(5)
  )
  val exampleTreeWithSmartConstructors = Tree.branch(
    1,
    Tree.branch(
      2,
      Tree.leaf(3),
      Tree.leaf(4),
    ),
    Tree.leaf(5)
  )
  println(s"Tree: ${do10x[Tree](exampleTree)}")
  println(s"Tree: ${do10x(exampleTreeWithSmartConstructors)}")


  // Extension method - map
  import cats.syntax.functor._
  println(s"Tree: ${exampleTreeWithSmartConstructors.map(_ + 10)}")

  def do10X[F[_] : Functor](fa: F[Int]): F[Int] = fa.map(_ * 10)

  println(s"Tree: ${do10X(exampleTreeWithSmartConstructors)}")
}
