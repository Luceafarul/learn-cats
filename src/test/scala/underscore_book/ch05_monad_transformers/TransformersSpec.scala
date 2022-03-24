package underscore_book.ch05_monad_transformers

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.applicative._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

class TransformersSpec extends AsyncWordSpec with Matchers {
  "Transformers" should {
    import book.ch05_monad_transformers.exercise.Transformers._

    "return power level if autobot exist" in {
      powerLevel("Jazz").value.map { f => f shouldBe Right(6) }
    }

    "return error message if autobot does not exist" in {
      powerLevel("Ironhide").value.map { f =>
        f shouldBe Left("Common error: Ironhide unreachable")
      }
    }

    "return Response true if power level enough for special move" in {
      canSpecialMove("Jazz", "Hot Rod").value.map { f =>
        f shouldBe Right(true)
      }
    }

    "return Response false if power level does not enough for special move" in {
      canSpecialMove("Jazz", "Bumblebee").value.map { f =>
        f shouldBe Right(false)
      }
    }

    "return error message if one of the autobot does not exist" in {
      canSpecialMove("Jazz", "Ironhide").value.map { f =>
        f shouldBe Left("Common error: Ironhide unreachable")
      }
    }

    "return success report message if 2 autobots can roll out" in {
      tacticalReport("Bumblebee", "Hot Rod") shouldBe "Bumblebee and Hot Rod are ready to roll out!"
    }

    "return fail report message if autobots needs recharge" in {
      tacticalReport("Jazz", "Bumblebee") shouldBe "Jazz and Bumblebee need a recharge."
    }

    "return error report message if one of 2 autobots does not exist" in {
      tacticalReport("Jazz", "Ironhide") shouldBe "Common error: Ironhide unreachable"
    }
  }
}
