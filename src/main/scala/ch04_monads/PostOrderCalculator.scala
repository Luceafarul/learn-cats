package ch04_monads

import cats.data.State
import cats.syntax.applicative._

object PostOrderCalculator {
  type CaclulationState[A] = State[List[Int], A]

  // TODO:
  // 1. Remove duplication code
  // 2. rewrite updateStack()
  def evalOne(symbol: String): CaclulationState[Int] = State[List[Int], Int] {
    oldStack =>
      def updateStack(): List[Int] =
        if (symbol == "+" || symbol == "-" || symbol == "*" || symbol == "/")
          oldStack
        else symbol.toInt +: oldStack
      var newStack = updateStack
      val result = symbol match {
        case "+" =>
          if (oldStack.size >= 2) {
            val res = oldStack.head + oldStack.drop(1).head
            newStack = res +: oldStack.drop(2)
            res
          } else oldStack.head
        case "-" =>
          if (oldStack.size >= 2) {
            val res = oldStack.head - oldStack.drop(1).head
            newStack = res +: oldStack.drop(2)
            res
          } else -(oldStack.head)
        case "*" =>
          if (oldStack.size >= 2) {
            val res = oldStack.head * oldStack.drop(1).head
            newStack = res +: oldStack.drop(2)
            res
          } else oldStack.head
        case "/" =>
          if (oldStack.size >= 2) {
            val res = oldStack.head / oldStack.drop(1).head
            newStack = res +: oldStack.drop(2)
            res
          } else oldStack.head
        case _ => symbol.toInt
      }
      (newStack, result)
  }

  def evalAll(input: List[String]): CaclulationState[Int] =
    input.foldLeft(0.pure[CaclulationState]) { (state, elem) =>
      state.flatMap(_ => evalOne(elem))
    }
}
