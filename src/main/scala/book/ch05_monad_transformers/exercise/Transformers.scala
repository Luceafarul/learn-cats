package book.ch05_monad_transformers.exercise

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.applicative._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

object Transformers extends App {

  private val powerLevels = Map("Jazz" -> 6, "Bumblebee" -> 8, "Hot Rod" -> 10)
  private val ComboPower = 15

  type Response[A] = EitherT[Future, String, A] // Future[Either[String, A]] -> EitherT[Future, String, A]

  def powerLevel(ally: String): Response[Int] = {
    powerLevels.get(ally) match {
      case Some(value) => value.pure[Response] // EitherT.right(Future(avg)) why need add future?
      case None        => EitherT.leftT(s"Error: $ally unreachable") // EitherT.left(Future(s"$ally unreachable"))
    }
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      pl1 <- powerLevel(ally1)
      pl2 <- powerLevel(ally2)
    } yield pl1 + pl2 > ComboPower

  def tacticalReport(ally1: String, ally2: String): Unit =
    canSpecialMove(ally1, ally2).value.onComplete {
      case Success(Right(true))  => println(s"$ally1 and $ally2 are ready to roll out!")
      case Success(Right(false)) => println(s"$ally1 and $ally2 need a recharge.")
      case Success(Left(error))  => println(error)
    }

  tacticalReport("Jazz", "Bumblebee")
  tacticalReport("Bumblebee", "Hot Rod")
  tacticalReport("Jazz", "Ironhide")

  println(powerLevel("Jazz"))
}
