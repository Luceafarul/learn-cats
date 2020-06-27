package ch04_monads

import cats.data.State

object PostOrderCalculator {
  type CaclulationState[A] = State[List[Int], A]

  def evalOne(symbol: String): CaclulationState[Int] = State[List[Int], Int] { oldStack =>
    def updateStack(): List[Int] = if (symbol == "+" || symbol == "-" || symbol == "*" || symbol == "/") oldStack else oldStack :+ symbol.toInt
    val newStack = updateStack
    val result = symbol match {
        case "+" => if (oldStack.size >= 2) oldStack.dropRight(1).last + oldStack.last else oldStack.head
        case "-" => if (oldStack.size >= 2) oldStack.dropRight(1).last - oldStack.last else -(oldStack.head)
        case "*" => if (oldStack.size >= 2) oldStack.dropRight(1).last * oldStack.last else oldStack.head
        case "/" => if (oldStack.size >= 2) oldStack.dropRight(1).last - oldStack.last else oldStack.head
        case _ => symbol.toInt
    }
    (newStack, result)
  }
}
