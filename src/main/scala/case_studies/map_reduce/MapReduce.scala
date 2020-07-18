package case_studies.map_reduce

import cats.Monoid
import cats.syntax.semigroup._
import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object MapReduce {
  def foldMap[A, B: Monoid](seq: Vector[A])(f: A => B): B =
    // There are my implementation of foldMap body
    // seq.foldLeft(Monoid[B].empty)((accum, elem) => Monoid[B].combine(accum, f(elem)))
    // seq.map(f).foldLeft(Monoid[B].empty)((accum, elem) => Monoid[B].combine(accum, elem))
    // There are solution from book
    // seq.map(f).foldLeft(Monoid[B].empty)(_ |+| _)
    seq.foldLeft(Monoid[B].empty)(_ |+| f(_))

  def parallelFoldMap[A, B: Monoid](seq: Vector[A])(f: A => B): Future[B] = {
    val cpusNumber = Runtime.getRuntime.availableProcessors
    // 1. Divide to batch for each CPU
    seq
      .grouped(cpusNumber)
      // 2. map over batches in parallel
      .map(vs => Future(foldMap(vs)(f)))
      // 3. reduce each batches in parallel
      .toList
      .sequence
      .map(ls => ls.foldLeft(Monoid[B].empty)(_ |+| _))
    // 4. reduce the batches
  }
}
