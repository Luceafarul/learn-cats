package courses.rockthejvm.part2

import cats.MonadError

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object _5_UsingMonads extends App {

  import cats.Monad

  val monadList = Monad[List]
  val aSimpleList = monadList.pure(3)
  val anExtendedList = monadList.flatMap(aSimpleList)(n => List(n, n + 1, n * 2))

  println(aSimpleList)
  println(anExtendedList)

  val aManualEither: Either[String, Int] = Right(13)
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  val loadingMonad = Monad[LoadingOr]
  val anEither = loadingMonad.pure(73)
  val aChangedLoading = loadingMonad.flatMap(anEither)(n => if (n % 2 == 0) Right(n + 1) else Left("Loading value..."))

  println(anEither)
  println(aChangedLoading)

  // Example: online store
  final case class OrderStatus(orderId: Long, status: String)

  object StoreAPI {
    def orderStatus(orderId: Long): LoadingOr[OrderStatus] =
      Right(OrderStatus(orderId, "Ready to ship"))

    def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
      if (orderStatus.orderId > 1000) Left("Not available yet, please wait...")
      else Right("Amsterdam, NL")
  }

  val orderIdOne = 457L
  val orderIdTwo = 1023L

  val orderLocationOne =
    loadingMonad.flatMap(StoreAPI.orderStatus(orderIdOne))(orderStatus => StoreAPI.trackLocation(orderStatus))
  val orderLocationTwo = for {
    orderStatus <- StoreAPI.orderStatus(orderIdTwo)
    location <- StoreAPI.trackLocation(orderStatus)
  } yield location

  println(orderLocationOne)
  println(orderLocationTwo)

  // Exercise: the service layer API of a web app
  final case class Connection(host: String, port: String)

  val config = Map(
    "host" -> "localhost",
    "port" -> "8000",
  )

  trait HttpService[M[_]] {
    def connection(config: Map[String, String]): M[Connection]

    def request(connection: Connection, payload: String): M[String]
  }

  // Requirements:
  // - if the host and port are found in the config map, then we'll return a M that containing
  //   a connection with those values, otherwise the method will fail, according to the
  //   logic of the type M (for Try it will return a Failure, for Option - None,
  //   for Future it will be a failed Future and for Either it will be Left).
  // - the request method returns a M containing the string:
  //    - "request (payload) has been accepted", if the payload is less than 20 characters,
  //    - otherwise the method will fail, according to the logic of the type M.

  // Use this two imports or just - import cats.implicits._
  import cats.syntax.functor._
  import cats.syntax.flatMap._

  def getResponse[M[_] : Monad](httpService: HttpService[M], config: Map[String, String], payload: String): M[String] =
    for {
      connection <- httpService.connection(config)
      response <- httpService.request(connection, payload)
    } yield response

  // Exercise: provide a real implementation of HttpService using Try, Option, Future, Either.
  val tryHttpService: HttpService[Try] = new HttpService[Try] {
    def connection(config: Map[String, String]): Try[Connection] =
      (config.get("host"), config.get("port")) match {
        case (Some(host), Some(port)) => Success(Connection(host, port))
        case (None, Some(_)) => Failure(new IllegalArgumentException("host parameter does not exit in config map"))
        case (Some(_), None) => Failure(new IllegalArgumentException("port parameter does not exit in config map"))
        case _ => Failure(new IllegalArgumentException("host and port parameter does not exit in config map"))
      }

    def request(connection: Connection, payload: String): Try[String] =
      if (payload.length < 20) Success(s"request: $payload has been accepted")
      else Failure(new Exception("Payload is too large"))
  }

  val optionHttpService: HttpService[Option] = new HttpService[Option] {
    def connection(config: Map[String, String]): Option[Connection] =
      for {
        host <- config.get("host")
        port <- config.get("port")
      } yield Connection(host, port)

    def request(connection: Connection, payload: String): Option[String] =
      if (payload.length < 20) Some(s"request: $payload has been accepted")
      else None
  }

  object FutureHttpService extends HttpService[Future] {

    import scala.concurrent.ExecutionContext.Implicits.global

    def connection(config: Map[String, String]): Future[Connection] =
      (config.get("host"), config.get("port")) match {
        case (Some(host), Some(port)) => Future(Connection(host, port))
        case (None, Some(_)) => Future.failed(new IllegalArgumentException("host parameter does not exit in config map"))
        case (Some(_), None) => Future.failed(new IllegalArgumentException("port parameter does not exit in config map"))
        case _ => Future.failed(new IllegalArgumentException("host and/or port parameter does not exit in config map"))
      }

    def request(connection: Connection, payload: String): Future[String] =
      if (payload.length < 20) Future(s"request: $payload has been accepted")
      else Future.failed(new Exception("Payload is too large"))
  }

  object EitherHttpService extends HttpService[ErrorOr] {
    def connection(config: Map[String, String]): ErrorOr[Connection] =
      (config.get("host"), config.get("port")) match {
        case (Some(host), Some(port)) => Right(Connection(host, port))
        case (None, Some(_)) => Left(new IllegalArgumentException("host parameter does not exit in config map"))
        case (Some(_), None) => Left(new IllegalArgumentException("port parameter does not exit in config map"))
        case _ => Left(new IllegalArgumentException("host and/or port parameter does not exit in config map"))
      }

    def request(connection: Connection, payload: String): ErrorOr[String] =
      if (payload.length < 20) Right(s"request: $payload has been accepted")
      else Left(new Exception("Payload is too large"))
  }

  val eitherResult = for {
    connection <- EitherHttpService.connection(config)
    request <- EitherHttpService.request(connection, "Simple request...")
  } yield request

  println(eitherResult)

  val tryResult = for {
    connection <- tryHttpService.connection(config)
    request <- tryHttpService.request(connection, "This is simple request...")
  } yield request

  println(tryResult)

  // Generic http service
  class GenericHttpService[F[_] : Monad] extends HttpService[F] {

    import cats.implicits._

    def connection(config: Map[String, String]): F[Connection] = {
      val connection = for {
        host <- config.get("host")
        port <- config.get("port")
      } yield Connection(host, port)

      connection match {
        case Some(value) => value.pure[F]
        case _ => throw new IllegalArgumentException()
      }
    }

    def request(connection: Connection, payload: String): F[String] =
      if (payload.length < 20) s"request: $payload has been accepted".pure[F]
      else throw new Exception("Payload is too large")
  }

  object GenericHttpService {
    def apply[F[_] : Monad] = new GenericHttpService[F]
  }

  val genericResult = for {
    connection <- GenericHttpService[Option].connection(config)
    request <- GenericHttpService[Option].request(connection, "This is simple request...")
  } yield request

  println(genericResult)

  // Hmm... looks really bad, because I throw exception in both method? How to handle this?
}
