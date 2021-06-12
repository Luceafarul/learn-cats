package underscore_book.case_studies.map_reduce

import cats.Monoid
import cats.Foldable
import cats.Traverse
import cats.instances.future._
import cats.instances.list._
import cats.instances.vector._
import cats.syntax.semigroup._
import cats.syntax.traverse._
import cats.syntax.foldable._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import cats.Foldable

object MapReduce {
  def foldMap[A, B: Monoid](seq: Vector[A])(f: A => B): B =
    // There are my implementation of foldMap body
    // seq.foldLeft(Monoid[B].empty)((accum, elem) => Monoid[B].combine(accum, f(elem)))
    // seq.map(f).foldLeft(Monoid[B].empty)((accum, elem) => Monoid[B].combine(accum, elem))
    // There are solution from book
    // seq.map(f).foldLeft(Monoid[B].empty)(_ |+| _)
    seq.foldLeft(Monoid[B].empty)(_ |+| f(_))

  def foldMapCats[A, B: Monoid](seq: Vector[A])(f: A => B): B =
    seq.foldMap(f)

  def parallelFoldMap[A, B: Monoid](seq: Vector[A])(f: A => B): Future[B] = {
    val coresNumber = Runtime.getRuntime.availableProcessors
    val groupsNumber = (1.0 * seq.size / coresNumber).ceil.toInt

    seq
      .grouped(groupsNumber)
      .map(vs => Future(foldMap(vs)(f)))
      .toList
      .sequence
      .map(ls => ls.foldLeft(Monoid[B].empty)(_ |+| _))
  }

  def parallelFoldMapCats[A, B: Monoid](seq: Vector[A])(f: A => B): Future[B] = {
    val coresNumber = Runtime.getRuntime.availableProcessors
    val groupsNumber = (1.0 * seq.size / coresNumber).ceil.toInt

    seq
      .grouped(groupsNumber)
      .toVector
      .traverse(vs => Future(vs.foldMap(f)))
      .map(vs => vs.combineAll)
  }
}
