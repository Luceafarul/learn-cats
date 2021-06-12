package underscore_book.ch03_functors

import cats.Functor
import cats.syntax.functor._
import org.scalatest.{WordSpec, Matchers}

class CustomFunctorSpec extends WordSpec with Matchers {
  "CustomFunctor" should {
    "add 1 to value in the Box" in {
      val boxWithInt = Box(13)

      boxWithInt.map(value => value + 1) shouldBe Box(14)
    }

    "add 3 for every value in the Tree" in {
      val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

      tree.map(n => n + 3) shouldBe Branch(Branch(Leaf(4), Leaf(5)), Leaf(6))
    }

    "mutilple string by 3 in the Tree" in {
      val tree = Tree.branch(
        Tree.leaf("hi"),
        Tree.branch(Tree.leaf("we"), Tree.leaf("are"))
      )

      tree.map(s => s * 3) shouldBe Branch(
        Leaf("hihihi"),
        Branch(Leaf("wewewe"), Leaf("areareare"))
      )
    }
  }
}
