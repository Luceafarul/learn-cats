package underscore_book.ch05_monad_transformers.exercise

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.applicative._
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object Transformers {

  private val powerLevels = Map("Jazz" -> 6, "Bumblebee" -> 8, "Hot Rod" -> 10)
  private val powerLevelForSpecialMove = 15

  type Response[A] = EitherT[Future, String, A] // Future[Either[String, A]] -> EitherT[Future, String, A]

  def powerLevel(autobot: String): Response[Int] = {
    powerLevels.get(autobot) match {
      case Some(value) => value.pure[Response]
      case None        => EitherT.leftT(s"Common error: $autobot unreachable")
    }
  }

  def canSpecialMove(abot1: String, abot2: String): Response[Boolean] =
    for {
      pl1 <- powerLevel(abot1)
      pl2 <- powerLevel(abot2)
    } yield pl1 + pl2 > powerLevelForSpecialMove

  def tacticalReport(abot1: String, abot2: String): String = {
    val stack = canSpecialMove(abot1, abot2).value

    Await.result(stack, 1.seconds) match {
      case Right(true)  => s"$abot1 and $abot2 are ready to roll out!"
      case Right(false) => s"$abot1 and $abot2 need a recharge."
      case Left(error)  => error
    }
  }
}
