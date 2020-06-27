package ch04_monads

import cats.data.State
import org.scalatest.{WordSpec, Matchers}

class PostOrderCalculatorSpec extends WordSpec with Matchers {
  "PostOrderCalculator" should {
    import PostOrderCalculator._

    "evaluate one symbol with evalOne method" in {
      val result = evalOne("123").runA(Nil).value
      result shouldBe 123
    }

    "evaluate comlex program using evalOne and combine it via for-comprehension" in {
      val program = for {
        _ <- evalOne("12")
        _ <- evalOne("13")
        answer <- evalOne("+")
      } yield answer
      val result = program.runA(Nil).value

      result shouldBe 25
    }
  }
}
