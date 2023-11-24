package free

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class FreeCalculator extends AnyFreeSpec with Matchers {
  import FreeCalculator.calc._

  "add two number with calc" in {
    val program = for {
      sum1 <- plus(1, 2)
      sum2 <- plus(3, 4)
      res <- plus(sum1, sum2)
    } yield res

    program.foldMap(FreeCalculator.calcCompiler) shouldBe 10
  }

  "multiple two number with calc" in {
    val program = for {
      m1 <- multiple(2, 2)
      m2 <- multiple(3, 4)
      res <- multiple(m1, m2)
    } yield res

    program.foldMap(FreeCalculator.calcCompiler) shouldBe 48
  }

  "calc operation" in {
    val program = for {
      sum <- plus(2, 2)
      div <- division(6, 2)
      mul <- multiple(sum, div)
      res <- minus(mul, 5)
    } yield res

    program.foldMap(FreeCalculator.calcCompiler) shouldBe 7
  }
}

object FreeCalculator {
  trait Calculator[T]
  final case class Plus(a: Int, b: Int) extends Calculator[Int]
  final case class Minus(a: Int, b: Int) extends Calculator[Int]
  final case class Division(a: Int, b: Int) extends Calculator[Int]
  final case class Multiple(a: Int, b: Int) extends Calculator[Int]

  object calc {
    import cats.free.Free
    type Calc[A] = Free[Calculator, A]

    import cats.free.Free

    def plus(a: Int, b: Int): Calc[Int] =
      Free.liftF[Calculator, Int](Plus(a, b))

    def minus(a: Int, b: Int): Calc[Int] =
      Free.liftF[Calculator, Int](Minus(a, b))

    def division(a: Int, b: Int): Calc[Int] =
      Free.liftF[Calculator, Int](Division(a, b))

    def multiple(a: Int, b: Int): Calc[Int] =
      Free.liftF[Calculator, Int](Multiple(a, b))
  }

  import cats.arrow.FunctionK
  import cats.{~>, Id}

  def calcCompiler: Calculator ~> Id = new FunctionK[Calculator, Id] {

    def apply[A](fa: FreeCalculator.Calculator[A]): Id[A] =
      fa match {
        case FreeCalculator.Plus(a, b)     => a + b
        case FreeCalculator.Minus(a, b)    => a - b
        case FreeCalculator.Division(a, b) => a / b
        case FreeCalculator.Multiple(a, b) => a * b
      }
  }
}
