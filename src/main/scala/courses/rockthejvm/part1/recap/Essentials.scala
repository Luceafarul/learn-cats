package courses.rockthejvm.part1.recap

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object Essentials extends App {
  // Singleton and objects
  object USD

  case object EUR

  println(USD)
  println(EUR)

  sealed trait Currency

  object Currency {
    case object USD extends Currency

    case object EUR extends Currency

    case object UAH extends Currency
  }

  println(Currency.UAH)

  // Functional programming
  val incrementer: Int => Int = x => x + 1
  println(incrementer(2))

  // map, flatMap, filter
  val list = List(1, 2, 3)
  println(list.map(incrementer))
  println(list.flatMap(n => List(n, incrementer(n))))

  // for-comprehensions
  val product = for {
    l1 <- List(1, 2, 3)
    l2 <- List('a', 'b', 'c')
  } yield (l1, l2)
  println(product)

  // Futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
  val aFuture = Future {
    73
  }

  // Wait for completion (async)
  aFuture.onComplete {
    case Success(value) => println(s"The async value: $value")
    case Failure(exception) => println(s"Computation failed with: $exception")
  }

  val anotherFuture = aFuture.map(_ + 10)

  println(anotherFuture)
  anotherFuture.onComplete {
    case Success(value) => println(s"The async value: $value")
    case Failure(exception) => println(s"Computation failed with: $exception")
  }
}
