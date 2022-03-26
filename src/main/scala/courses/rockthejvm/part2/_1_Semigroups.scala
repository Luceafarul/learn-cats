package courses.rockthejvm.part2

object _1_Semigroups extends App {
  // Semigroups COMBINE elements of the same type

  import cats.Semigroup

  val naturalIntSemigroup = Semigroup[Int]
  val intCombination = naturalIntSemigroup.combine(70, 3)
  println(s"Int combination: $intCombination")

  val naturalStringSemigroup = Semigroup[String]
  val stringCombination = naturalStringSemigroup.combine("Hello, ", "Cats!")
  println(stringCombination)

  def reduceInts(xs: List[Int]): Int = xs.reduce(naturalIntSemigroup.combine)

  def reduceStrings(xs: List[String]): String = xs.reduce(naturalStringSemigroup.combine)

  val numbers = (1 to 10).toList
  val words = List("Hello, ", "darkness ", "my ", "old ", "friend.")
  println(s"Reduce ints: ${reduceInts(numbers)}")
  println(s"Reduce strings: ${reduceStrings(words)}")

  // General API
  def reduce[T](xs: List[T])(implicit semigroup: Semigroup[T]): T =
    xs.reduce(semigroup.combine)

  println(s"General reduce Int: ${reduce(numbers)}")
  println(s"General reduce String: ${reduce(words)}")

  import cats.syntax.option._

  val numberOptions = numbers.map(_.some)
  println(s"General reduce Options Int: ${reduce(numberOptions)}")

  // Support custom type
  final case class Expense(id: Long, amount: Double)

  object Expense {
    implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense] { (e1, e2) =>
      Expense(Math.max(e1.id, e2.id) + 1, e2.amount + e1.amount)
    }
  }

  val e1 = Expense(1, 2.99)
  val e2 = Expense(2, 7.21)

  import cats.syntax.semigroup._

  println(s"Combine expenses: ${e1 |+| e2}")

  // This method signature identical with reducer[T](xs: List[T])(implicit semigroup: Semigroup[T]): T
  def reducer[T: Semigroup](xs: List[T]): T = xs.reduce(_ |+| _)

  println(s"Reducer expenses: ${reducer(List(e1, e2, Expense(3, 2.31)))}")
}
