package underscore_book.ch04_monads

import book.ch04_monads.Monads
import org.scalatest.{Matchers, WordSpec}

class MonadSpec extends WordSpec with Matchers {
  "Monad" should {
    val bigList = List.fill[BigInt](50000)(7)
    "foldRigh should throw an StackOverflowError on deep recursion" in {
      assertThrows[StackOverflowError] {
        Monads.foldRight(bigList, BigInt(1))((a, b) => a * b)
      }
    }

    "foldRightEval should not throw an StackOverflowError on deep recursion" in {
      noException should be thrownBy Monads
        .foldRightEval(bigList, BigInt(1))((a, b) => a * b)
        .value
    }
  }
}
