package underscore_book.ch04_monads

import book.ch03_functors.Tree
import Tree._
import cats.syntax.applicative._
import org.scalatest.{WordSpec, Matchers}

class CustomMonadSpec extends WordSpec with Matchers {
  final case class Person(name: String, age: Int)

  "TreeMonad" should {
    "wrap value with pure method" in {
      val person = Person("Marcus", 73)
      val result = person.pure

      result shouldBe leaf(person)
    }

    "transform tree content with flatMap" in {
      val p1 = Person("Marcus", 73)
      val p2 = Person("Antonii", 37)
      val p3 = Person("Dekart", 23)
      val persons: Tree[Person] = branch(branch(leaf(p1), leaf(p2)), leaf(p3))

      val result = treeMonad.flatMap(persons)(p => leaf(p.name))

      result shouldBe branch(
        branch(leaf(p1.name), leaf(p2.name)),
        leaf(p3.name)
      )
      result.isInstanceOf[Tree[String]] shouldBe true
    }
  }
}
