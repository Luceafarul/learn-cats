package underscore_book.examples.type_classes

object CollapseList extends App {
  def sumInts(xs: List[Int]): Int = xs.foldRight(0)(_ + _)
  def concatString(xs: List[String]): String = xs.foldRight("")(_ + _)
  def unionSet[A](xs: List[Set[A]]): Set[A] =
    xs.foldRight(Set.empty[A])(_ union _)

  val sum = sumInts(List(1, 2, 3, 4, 5))
  val concat = concatString(List("a", "b", "c", "d", "e"))
  val union = unionSet(List(Set(1, 2), Set(3, 4), Set(4, 5)))

  println(sum)
  println(concat)
  println(union)

  def combineAll[A](list: List[A]): A = ???
}
