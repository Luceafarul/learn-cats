package underscore_book.ch04_monads
import scala.collection.immutable.Nil

import cats.Eval

object Monads {
  // Eval
  def foldRight[A, B](xs: List[A], acc: B)(f: (A, B) => B): B = xs match {
    case head :: tail => f(head, foldRight(tail, acc)(f))
    case Nil          => acc
  }

  def foldRightEval[A, B](xs: List[A], acc: B)(f: (A, B) => B): Eval[B] = xs match {
      case head :: tail => Eval.defer(foldRightEval(tail, f(head, acc))(f))
      case Nil          => Eval.now(acc)
    }

  // Eval solution from book
  def foldRightEvalSolution[A, B](xs: List[A], acc: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = xs match {
    case head :: tail => Eval.defer(f(head, foldRightEvalSolution(tail, acc)(f)))
    case Nil => acc
  }

  def foldMapSolution[A, B](xs: List[A], acc: B)(f: (A, B) => B): B =
    foldRightEvalSolution(xs, Eval.now(acc)) { (a, b) => b.map(f(a, _)) }.value
}
