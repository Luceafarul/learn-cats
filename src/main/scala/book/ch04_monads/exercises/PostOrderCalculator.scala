package book.ch04_monads.exercises

import cats.data.State
import cats.implicits._

import scala.util.Try

object PostOrderCalculator {

  type CalculatorState[A] = State[List[Int], A]

  sealed trait Operator
  case object Plus extends Operator
  case object Minus extends Operator
  case object Divide extends Operator
  case object Multiply extends Operator

  def operator(s: String): Option[Operator] =
    s match {
      case "+" => Plus.some
      case "-" => Minus.some
      case "/" => Divide.some
      case "*" => Multiply.some
      case _   => None
    }

  def isNumber(s: String): Boolean =
    Try(s.toInt).isSuccess

  def isOperator(s: String): Boolean =
    operator(s).isDefined

  def evalOne(symbol: String): CalculatorState[Int] = State { stack =>
    symbol match {
      case n if isNumber(n) => (stack :+ n.toInt, n.toInt)
      case s if isOperator(s) =>
        operator(s) match {
          case Some(operator) =>
            operator match {
              case Plus     => evaluate(_ + _, stack)
              case Minus    => evaluate(_ - _, stack)
              case Divide   => evaluate(_ / _, stack)
              case Multiply => evaluate(_ * _, stack)
            }
          case None => (stack, stack.last)
        }
      case _ => (stack, stack.last)
    }
  }

  private def evaluate(f: (Int, Int) => Int, stack: List[Int]): (List[Int], Int) =
    stack match {
      case a :: b :: tail =>
        val result = f(a, b)
        (tail :+ result, result)
      case stack => (stack, 0)
    }

  def evalAll(input: List[String]): CalculatorState[Int] = {
    input.foldLeft(0.pure[CalculatorState]) { (state, elem) =>
      state.flatMap(_ => evalOne(elem))
    }
  }

  def evalInput(s: String): CalculatorState[Int] =
    evalAll(s.split(" ").toList)
}
