package cats_docs.doc00_type_classes

object TypeClassExample extends App {
  // Collapsing lists
  def sumInts(list: List[Int]): Int = list.foldRight(0)(_ + _)

  def concatStrings(list: List[String]): String = list.foldRight("")(_ + _)

  def unionSet[A](list: List[Set[A]]): Set[A] = list.foldRight(Set.empty[A])(_ union _)

  // All of these follow the same pattern:
  // - an initial value (0, empty string, empty set)
  // - and a combining function (+, ++, union).
  // We’d like to abstract over this so we can write the function once
  // instead of once for every type so we pull out the necessary pieces into an interface.
  trait Monoid[A] {
    def empty: A
    def combine(x: A, y: A): A
  }

  // Add monoid implementation for Int
  implicit val indAdditionMonoid: Monoid[Int] = new Monoid[Int] {
    override def empty: Int = 0
    override def combine(x: Int, y: Int): Int = x + y
  }

  final case class Pair[A, B](first: A, second: B)

  object Pair {
    implicit def deriveMonoidPair[A, B](implicit A: Monoid[A], B: Monoid[B]): Monoid[Pair[A, B]] = new Monoid[Pair[A, B]] {
      override def empty: Pair[A, B] = Pair(A.empty, B.empty)
      override def combine(x: Pair[A, B], y: Pair[A, B]): Pair[A, B] = Pair(
        A.combine(x.first, y.first),
        B.combine(x.second, y.second)
      )

      implicit def tuple2Instance[A, B](implicit A: Monoid[A], B: Monoid[B]): Monoid[Pair[A, B]] = new Monoid[Pair[A, B]] {
        override def empty: Pair[A, B] = Pair(A.empty, B.empty)
        override def combine(x: Pair[A, B], y: Pair[A, B]): Pair[A, B] = Pair(
          A.combine(x.first, y.first),
          B.combine(x.second, y.second)
        )
      }
    }
  }

  // We can now write the functions above against this interface.
  def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A =
    list.foldRight(monoid.empty)(monoid.combine)

  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    override def empty: String = ""
    override def combine(x: String, y: String): String = x ++ y
  }

  println(combineAll(List(1, 2, 3, 4, 5))(indAdditionMonoid))
  println(combineAll(List("Hello", ", ", "world", "!")))
  println(combineAll(List(Pair(1, "hello"), Pair(2, " "), Pair(3, "world"))))
}
