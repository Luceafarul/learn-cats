package underscore_book.ch04_monads

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PostOrderCalculatorSpec extends AnyWordSpec with Matchers {
  "PostOrderCalculator" should {
    import book.ch04_monads.exercises.PostOrderCalculator._

    "evaluate one symbol with evalOne method" in {
      val result = evalOne("123").runA(Nil).value
      result shouldBe 123
    }

    "evaluate complex program using evalOne and combine it via for-comprehension" in {
      val program = for {
        _ <- evalOne("12")
        _ <- evalOne("13")
        answer <- evalOne("+")
      } yield answer
      val result = program.runA(Nil).value

      result shouldBe 25
    }

    "evaluate multi-stage expression using evalAll" in {
      val program = evalAll(List("1", "2", "3", "+", "+", "3", "*"))
      val result = program.runA(Nil).value

      result shouldBe 18
    }

    "evaluate input string to int result" in {
      val input = "1 3 3 + + 3 *"
      val result = evalInput(input).runA(Nil).value

      result shouldBe 21
    }
  }
}
