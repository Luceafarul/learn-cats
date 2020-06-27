package ch04_monads

import org.scalatest.{AsyncWordSpec, Matchers, Assertions}

import scala.concurrent._
import scala.concurrent.duration._
import org.scalatest.Succeeded

class LoggedMonadSpec extends AsyncWordSpec with Matchers {
  "LoggedMonad" should {
    import LoggedMonad._

    "return logged messages and result of factorial" in {
      val res = factorial(5)
      val (logs, result) = res.run

      logs shouldBe Vector(
        s"[${Thread.currentThread.getName}] Factorial 1 = 1",
        s"[${Thread.currentThread.getName}] Factorial 2 = 2",
        s"[${Thread.currentThread.getName}] Factorial 3 = 6",
        s"[${Thread.currentThread.getName}] Factorial 4 = 24",
        s"[${Thread.currentThread.getName}] Factorial 5 = 120"
      )
      result shouldBe 120
    }

    "return logged messages from appropriate thread and resuklt of factorial" in {
      val futureRes = Future.sequence(Vector(Future(factorial(5)), Future(factorial(5))))

      futureRes.map { vector =>
        vector.map { write =>
          val (logs, res) = write.run
          val threadName = logs.head.split(" ").head
          all(logs) should startWith(threadName)
          res shouldBe 120
        }
      }.map(listOfAssertions => assert(listOfAssertions.forall(_ == Succeeded)))
    }
  }
}