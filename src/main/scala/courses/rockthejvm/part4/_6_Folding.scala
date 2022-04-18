package courses.rockthejvm.part4

import cats.Eval
import cats.kernel.Monoid

object _6_Folding extends App {
  // Exercise: implement all in terms of folding foldLeft or foldRight
  object ListExercise {
    // Mem tips:
    // - foldLeft acc as left param, elem as right
    // - foldRight acc as right param, elem as left
    def map[A, B](xs: List[A])(f: A => B): List[B] =
      xs.foldLeft(List.empty[B])((acc, elem) => acc :+ f(elem))
    def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] =
      xs.foldLeft(List.empty[B])((acc, elem) => acc ++ f(elem))
    def filter[A](xs: List[A])(p: A => Boolean): List[A] =
      xs.foldLeft(List.empty[A])((acc, elem) =>
        if (p(elem)) acc :+ elem else acc
      )
    def combineAll[A](xs: List[A])(implicit monoid: Monoid[A]): A =
      xs.foldLeft(monoid.empty)((acc, elem) => monoid.combine(acc, elem))
  }

  import ListExercise._

  val numbers: List[Int] = (1 to 10).toList
  println(s"map: ${map(numbers)(_ + 1)}")
  println(s"flatMap: ${flatMap(numbers)(e => List(e, e + 1))}")
  println(s"filter: ${filter(numbers)(_ % 2 == 0)}")
  println(s"combineAll: ${combineAll(numbers)}")

  import cats.Foldable

  val sumList: Int = Foldable[List].foldLeft(numbers, 0)(_ + _)
  val sumOption: Int = Foldable[Option].foldLeft(Option(5), 7)(_ + _)
  println(s"Foldable[List].foldLeft: $sumList")
  println(s"Foldable[Option].foldLeft: $sumOption")

  // foldRight of Foldable is stack-safe
  val sumListAgain: Eval[Int] = Foldable[List].foldLeft(numbers, Eval.now(0)) {
    (acc, elem) => acc.map(_ + elem)
  }
  println(s"Foldable[List].foldRight: ${sumListAgain.value}")
  println(s"Foldable[List].combineAll: ${Foldable[List].combineAll(numbers)}")

  val plusOneSum: Int = Foldable[List].foldMap(numbers)(_ + 1)
  val stringCombine: String = Foldable[List].foldMap(numbers)(_.toString)
  println(s"Foldable[List].foldMap: $plusOneSum")
  println(s"Foldable[List].foldMap: $stringCombine")

  val nested = List(Vector(1, 2, 3, 4, 5), Vector(6, 7, 8), Vector(9))
  val combined = Foldable[List].compose(Foldable[Vector]).combineAll(nested)
  println(s"Foldable[List] compose Foldable[Vector] combineAll: $combined")

  // Extension methods:
  import cats.syntax.foldable._

  numbers.combineAll // require Foldable[List] and Monoid[Int]
  numbers.foldMap(_.toString) // require Foldable[List] and Monoid[String]
}
