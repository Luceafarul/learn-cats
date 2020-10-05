package examples.type_classes

object CollapseListWithMonoid extends App {
  trait Monoid[A] {
    def empty: A
    def combine(a: A, b: A): A
  }

  val intAdditionMonoid: Monoid[Int] = new Monoid[Int] {
    def empty: Int = 0
    def combine(a: Int, b: Int): Int = a + b
  }

  def combineAll[A](xs: List[A], monoid: Monoid[A]): A = xs.foldRight(monoid.empty)(monoid.combine)

  val sum = combineAll(List(1, 2, 3, 4, 5), intAdditionMonoid)

  println(sum)
}
