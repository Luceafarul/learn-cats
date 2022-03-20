package book.ch04_monads

import cats.data.State
import cats.syntax.applicative._

object PostOrderCalculator {
  type CaclulationState[A] = State[List[Int], A]

  def evalOne(symbol: String): CaclulationState[Int] = symbol match {
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case "/" => operator(_ / _)
    case _   => operand(symbol.toInt)
  }

  def evalAll(input: List[String]): CaclulationState[Int] =
    input.foldLeft(0.pure[CaclulationState]) { (state, elem) =>
      state.flatMap(_ => evalOne(elem))
    }

    def evalInput(input: String): Int = evalAll(input.split(" ").toList).runA(Nil).value

  private def operator(f: (Int, Int) => Int): CaclulationState[Int] =
    State[List[Int], Int] {
      case a :: b :: tail =>
        val res = f(a, b)
        (res +: tail, res)
      case _ => sys.error("Fail!")
    }

  private def operand(num: Int): CaclulationState[Int] = State[List[Int], Int] {
    state => (num +: state, num)
  }
}
