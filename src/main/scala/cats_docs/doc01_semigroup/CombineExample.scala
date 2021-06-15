package cats_docs.doc01_semigroup

import cats.kernel.Semigroup
import cats.implicits._

object CombineExample extends App {
  val map = Map("foo" -> Map("bar" -> 5))
  val anotherMap = Map("foo" -> Map("bar" -> 6))
  val combineMap = Semigroup[Map[String, Map[String, Int]]].combine(map, anotherMap)

  println(combineMap)

  val one: Option[Int] = Option(1)
  val two: Option[Int] = Some(2)
  val none: Option[Int] = None

  println(one |+| two)
  println(none |+| two)
  println(none |+| none)
  println(one |+| none)

  val x = 1
  val y = 2
  val z = 3

  println(Semigroup[Int].combine(x, y))
  println(Semigroup[Int].combine(x, Semigroup[Int].combine(y, z)))
  println(Semigroup[Int].combine(Semigroup[Int].combine(x, y), z))
  println(x |+| y |+| z)

  val map1 = Map("hello" -> 1, "world" -> 2, "!" -> 3)
  val map2 = Map("hello" -> 1, "cats" -> 2)

  println(map1 |+| map2)
  println(map1 ++ map2)

  final case class Foo(n: Int)
  object Foo {
    implicit def semigroupFoo: Semigroup[Foo] = (x: Foo, y: Foo) => Foo(x.n + y.n)
  }

  val foo1 = Foo(1)
  val foo2 = Foo(2)
  println(Semigroup[Foo].combine(foo1, foo2))

  // Example usage: Merging maps
  def optionCombine[A: Semigroup](a: A, opt: Option[A]): A =
    opt.map(_ |+| a).getOrElse(a)

  def merge[K, V: Semigroup](lhs: Map[K, V], rhs: Map[K, V]): Map[K, V] =
    lhs.foldLeft(rhs) {
      case (acc, (k, v)) => acc.updated(k, optionCombine(v, acc.get(k)))
    }

  val xm1 = Map('a' -> 1, 'b' -> 2, 'c' -> 3)
  val xm2 = Map('b' -> 4, 'c' -> 5, 'd' -> 6)

  val xm3 = merge(xm1, xm2)
  println(xm3)

  val ym1 = Map(1 -> List("hello"))
  val ym2 = Map(2 -> List("cats"), 1 -> List("world"))

  val ym3 = merge(ym1, ym2)
  println(ym3)

  println(Semigroup[Map[Char, Int]].combine(xm1, xm2) == xm3)
  println(Semigroup[Map[Int, List[String]]].combine(ym1, ym2) == ym3)

  // Exploiting laws: associativity
  val leftwards = List(1, 2, 3).foldLeft(0) { (acc, elem) =>
    println(s"Add elem: $elem to acc: $acc")
    acc |+| elem
  }
  println(s"Leftwards result: $leftwards")

  val rightwards = List(1, 2, 3).foldRight(0) { (elem, acc) =>
    println(s"Add elem: $elem to acc: $acc")
    acc |+| elem
  }
  println(s"Rightwards result: $rightwards")

  val list = List(1, 2, 3, 4, 5)
  val (left, right) = list.splitAt(2)
  val sumLeft = left.foldLeft(0)(_ |+| _)
  val sumRight = right.foldLeft(0)(_ |+| _)
  val result = sumLeft |+| sumRight
  println(s"Result of sumLeft and sumRight: $result")
}
