package cats_docs.doc06_foldable

import cats._
import cats.implicits._

object FoldableExample extends App {
  // foldLeft - is an eager left-associative fold on F using the given function
  println("foldLeft examples:")
  println(Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _))
  println(Foldable[List].foldLeft(List(1, 2, 3), 0)(_ - _))
  println(Foldable[List].foldLeft(List("a", "b", "c"), "")(_ + _))

  // foldLeft - is a lazy right-associative fold on F using the given function
  println("\nfoldRight examples:")
  val sumLazyResult =
    Foldable[List].foldRight(List(1, 2, 3), Now(0))((x, rest) =>
      Later(x + rest.value)
    )
  println(sumLazyResult.value)
  val minusLazyResult =
    Foldable[List].foldRight(List(1, 2, 3), Now(0))((x, rest) =>
      Later(x - rest.value)
    )
  println(minusLazyResult.value)

  // fold - called combineAll, combines every value in the foldable using the given Monoid instance.
  println("\nfold examples:")
  println(Foldable[List].fold(List("a", "b", "c")))
  println(Foldable[List].fold(List(1, 2, 3)))

  // foldMap - maps every A value into B and then combines them using the given Monoid[B] instance.
  println("\nfoldMap examples:")
  println(Foldable[List].foldMap(List("a", "b", "c"))(x => {
    println(x.length)
    x.length
  }))
  println(Foldable[List].foldMap(List(1, 2, 3))(x => {
    println(x.toString)
    x.toString
  }))

  // foldK - combines every value in the foldable using the given MonoidK[G] instance instead of Monoid[G]
  println("\nfoldK examples:")
  println(Foldable[List].foldK(List(List(1, 2), List(3, 4, 5))))
  println(Foldable[List].foldK(List(None, Option("two"), Option("three"))))
  println(Option.when(cond = true)(List(1, 2, 3)).foldK)
  println(Option.when(cond = false)(List(1, 2, 3)).foldK)

  // find - searches for the first element matching the predicate, if one exists.
  println("\nfind examples:")
  println(Foldable[List].find(List(1, 2, 3))(_ > 2))
  println(Foldable[List].find(List(1, 2, 3))(_ > 5))

  // exists - checks whether at least one element satisfies the predicate.
  println("\nexists examples:")
  println(Foldable[List].exists(List(1, 2, 3))(_ > 2))
  println(Foldable[List].exists(List(1, 2, 3))(_ > 5))

  // forall - checks whether all elements satisfy the predicate.
  println("\nforall examples:")
  println(Foldable[List].forall(List(1, 2, 3))(_ <= 3))
  println(Foldable[List].forall(List(1, 2, 3))(_ < 3))

  // toList - convert F[A] to List[A]
  println("\ntoList examples:")
  println(Foldable[List].toList(List(1, 2, 3)))
  println(Foldable[Option].toList(Option(73)))
  println(Foldable[Option].toList(None))

  // filter_ - convert F[A] to List[A] only including the elements that match a predicate.
  println("\nfilter_ examples:")
  println(Foldable[List].filter_(List(1, 2, 3))(_ < 3))
  println(Foldable[Option].filter_(Option(73))(_ == 73))
  println(Foldable[Option].filter_(Option(73))(_ != 73))
  println(Foldable[Option].filter_(Option.empty[Int])(_ > 1))

  // traverse_ - the foldable mapping A values to G[B],
  // and combining them using Applicative[G] and discarding the results.
  println("\ntraverse_ examples:")
  def parseInt(s: String): Option[Int] = {
    println(s"try to parse: $s")
    Either.catchOnly[NumberFormatException](s.toInt).toOption
  }

  println(Foldable[List].traverse_(List("1", "2", "3"))(parseInt))
  println(Foldable[List].traverse_(List("1", "a", "b"))(parseInt))

  // compose - compose Foldable[F[_]] and Foldable[G[_]] instances to obtain Foldable[F[G]].
  println("\ncompose examples:")
  val foldableListOption = Foldable[List].compose[Option]
  println(
    foldableListOption
      .fold(List(Option(1), Option(2), Option(3), Option(4), Option(5)))
  )
  println(
    foldableListOption
      .fold(List(Option("1"), Option("2"), None, Option("4"), Option("5")))
  )
  println(
    foldableListOption.fold(List(Option(1), None, None, Option(4), Option(5)))
  )

  // More Foldable methods
  println("\nother Foldable methods examples:")
  println(Foldable[List].isEmpty(List(1, 2, 3)))
  println(Foldable[List].dropWhile_(List(1, 2, 3))(_ < 2))
  println(Foldable[List].takeWhile_(List(1, 2, 3))(_ < 2))

  // foldA
  final case class Person(name: String)
  object Person {
    implicit val monoidPerson: Monoid[Person] = new Monoid[Person] {
      def empty: Person = Person("")
      def combine(x: Person, y: Person): Person = Person(x.name + y.name)
    }
  }
  println("\nfoldA examples:")
  println(Option(List(1, 2, 3)).foldA)
  println(Option.when(cond = true)(List(1, 2)).foldA)
  println(Option.when(cond = true)(List(Person("Ann"), Person("Mike"))).foldA)
  println(Option.when(cond = false)(List(1, 2)).foldA)
  println(Option.when(cond = false)(List(Person("Ann"), Person("Mike"))).foldA)
  println(Option(List.empty[Int]).foldA)
  println(Option.empty[List[Int]].foldA)
}
