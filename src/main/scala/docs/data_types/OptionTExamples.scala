package docs.data_types

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object OptionTExamples {
  // Future example
  val customGreeting: Future[Option[String]] = Future.successful(Some("Welcome back, Lola!"))

  val excitedGreeting = customGreeting.map(_.map(_ + "!"))

  val hasWelcom = customGreeting.map(_.filter(_.contains("Welcome")))

  val noWelcom = customGreeting.map(_.filterNot(_.contains("Welcome")))

  val withFallback = customGreeting.map(_.getOrElse("Hello, there!"))

  // How it works with OptionT
  import cats.data.OptionT
  import cats.syntax.all._

  // Here we can see that our Future[Option[String]] transform into OptionT[Future, String]
  // Option context moves to the braces
  // As we see below, we can drop map over Future and work with String
  val customGreetingT: OptionT[Future, String] = OptionT(customGreeting)

  val excitedGreetingT = customGreetingT.map(_ + "!")

  val hasWelcomT = excitedGreetingT.filter(_.contains("Welcome"))

  val noWelcomT = customGreetingT.filterNot(_.contains("Welcome"))

  val withFallbackT = customGreetingT.getOrElse("Hello, there!")

  // Lifting: From Option[A] and/or F[A] to OptionT[F, A]
  val greetingFO = Future.successful(Option("Hello"))

  val firstnameF = Future.successful("Jane")

  val lastnameO = Some("Doe")

  val liftedGreetingT =
    for {
      hello <- OptionT(greetingFO)
      first <- OptionT.liftF(firstnameF)
      last <- OptionT.fromOption[Future](lastnameO)
    } yield s"$hello, $first $last"

  val liftedResult = liftedGreetingT.value

  // Lifting: From A to OptionT[F,A]
  val greet = OptionT.pure[Future]("Hello")
  val greetAlt = OptionT.some[Future]("Hola")
  val failedGreet = OptionT.none[Future, String]

  // Beyound map
  val defaultGreeting: Future[String] = Future.successful("hello, there")

  val greetingF = customGreeting.flatMap { custom =>
    custom.map(Future.successful).getOrElse(defaultGreeting)
  }

  val greetingT = OptionT(customGreeting).getOrElseF(defaultGreeting)
}
