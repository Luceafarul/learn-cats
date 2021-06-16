package cats_docs.doc02_monoid

import cats.kernel.{Monoid, Semigroup}
import cats.implicits._

object MonoidExample extends App {
  println(Monoid[String].empty)
  println(Monoid[String].combineAll(List("a", "b", "c", "d")))
  println(Monoid[String].combineAll(List()))

  println(Monoid[Map[String, Int]].combineAll(List(Map("a" -> 1, "b" -> 2), Map("a" -> 3))))
  println(Monoid[Map[String, Int]].combineAll(List()))

  val list1 = List(1, 2, 3, 4, 5)

  // foldMap is method from cats, if we check implicitArgument command + shift + P (mac)
  println(list1.foldMap(identity)) // catsKernelStdGroupForInt
  println(list1.foldMap(i => i.toString)) // catsKernelStdMonoidForString
  println(list1.foldMap(i => (i, i.toString))) // catsKernelStdMonoidForTuple2 -> and 2 types above

  val x = 1
  println(Monoid[Int].combine(x, Monoid[Int].empty))
  println(Monoid[Int].combine(Monoid[Int].empty, x))

  // Example usage: Collapsing a list
  // This function is provided in Cats as Monoid.combineAll
  def combineAll[A: Monoid](xs: List[A]): A = xs.foldLeft(Monoid[A].empty)(Monoid[A].combine)

  println(combineAll(List(1, 2, 3)))
  println(combineAll(List("hello", " ", "world", "!")))
  println(combineAll(List(Map('a' -> 1), Map('a' -> 2, 'b' -> 3), Map('b' -> 4, 'c' -> 5))))
  println(combineAll(List(Set(1, 2), Set(2, 3, 4, 5))))

  // The Option monoid
  final case class NonEmptyListExample[A](head: A, tail: List[A]) {
    def ++(other: NonEmptyListExample[A]): NonEmptyListExample[A] = NonEmptyListExample(head, tail ++ other.toList)
    def toList: List[A] = head :: tail
  }
  object NonEmptyListExample {
    implicit def nonEmptyListSemigroup[A]: Semigroup[NonEmptyListExample[A]] = new Semigroup[NonEmptyListExample[A]] {
      override def combine(x: NonEmptyListExample[A], y: NonEmptyListExample[A]): NonEmptyListExample[A] = x ++ y
    }
  }

  // How then can we collapse a List[NonEmptyList[A]]?
  // For such types that only have a Semigroup we can lift into Option to get a Monoid
  implicit def optionMonoid[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def empty: Option[A] = None
    override def combine(x: Option[A], y: Option[A]): Option[A] =
      x match {
        case None => y
        case Some(xValue) => y match {
          case None => x
          case Some(yValue) => Some(xValue.combine(yValue))
        }
      }
  }

  import cats.data.NonEmptyList

  val list2 = List(NonEmptyList(1, List(2, 3)), NonEmptyList(4, List(5, 6)))
  val lifted = list2.map(nel => Option(nel))
  println(Monoid.combineAll(lifted)(optionMonoid))
  // This lifting and combining of Semigroups into Option is provided by Cats as Semigroup.combineAllOption
  println(Semigroup.combineAllOption(list2))
}
