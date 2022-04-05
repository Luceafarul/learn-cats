package courses.rockthejvm.part3

object _1_Readers extends App {

  // Configuration file => initial data structure
  // - a DB layer
  // - an HTTP layer
  // - a business logic layer
  final case class Configuration(dbUser: String, dbPass: String, host: String, port: Int, emailReplyTo: String)

  final case class DBConnection(username: String, password: String) {
    def orderStatus(orderId: Long): String =
      "dispatched" // SELECT * FROM the DB table and return the status of the orderID

    def lastOrderId(username: String): Long =
      12345 // SELECT max(orderId) from table where username = username
  }

  final case class HTTPService(host: String, port: Int) {
    // This would start the actual server
    def start(): Unit = println("Server started...")
  }

  // Bootstrap:
  val config = Configuration("test", "test1!", "localhost", 8080, "test@email.com")

  // Cats Reader:
  // type Reader[-A, B] = ReaderT[Id, A, B]
  // type ReaderT[F[_], -A, B] = Kleisli[F, A, B]
  // Kleisli is Represents a function A => F[B].
  // final case class Kleisli[F[_], -A, B](run: A => F[B]) { self => ... }

  import cats.Id
  import cats.data.Reader

  val dbReader: Reader[Configuration, DBConnection] = Reader(config => DBConnection(config.dbUser, config.dbPass))
  val dbConnection: Id[DBConnection] = dbReader.run(config)

  // Reader[I, O]
  val userOrderStatusReader: Reader[Configuration, String] = dbReader.map(connection => connection.orderStatus(73))
  val userOrderStatus: String = userOrderStatusReader.run(config)

  def lastOrderStatus(username: String): String = {
    val result = for {
      lastOrderId <- dbReader.map(_.lastOrderId(username))
      lastOrderStatus <- dbReader.map(_.orderStatus(lastOrderId))
    } yield lastOrderStatus

    result.run(config)
  }

  println(lastOrderStatus("test"))

  // Exercise:
  //  - fetch the status of the last order
  //  - email them with the Email Service:
  //    "Your last order has the status: $status
  final case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String): String = s"From: $emailReplyTo; to: $address >>> $contents"
  }

  val emailServiceReader: Reader[Configuration, EmailService] = Reader(config => EmailService(config.emailReplyTo))

  def emailUser(username: String, userEmail: String): String = {
    val mail = for {
      lastOrderId <- dbReader.map(_.lastOrderId(username))
      orderStatus <- dbReader.map(_.orderStatus(lastOrderId))
      content = s"Your last order has status: $orderStatus"
      mail <- emailServiceReader.map(_.sendEmail(userEmail, content))
    } yield mail

    mail.run(config)
  }

  println(emailUser("test", "some@mail.com"))

  def solutionEmailUser(username: String, userEmail: String): String = {
    val emailServiceReader: Reader[Configuration, EmailService] = Reader(config => EmailService(config.emailReplyTo))
    val emailReader: Reader[Configuration, String] =
      for {
        lastOrderId <- dbReader.map(_.lastOrderId(username))
        orderStatus <- dbReader.map(_.orderStatus(lastOrderId))
        emailService <- emailServiceReader
      } yield emailService.sendEmail(userEmail, s"Your last order has status: $orderStatus")

    emailReader.run(config)
  }

  println(solutionEmailUser("test", "some@mail.com"))

  // Pattern:
  // 1. Create the initial data structure
  // 2. Create a reader which specifies how that data structure will be manipulated later
  // 3. Can then map & flatMap the reader to produce derived information
  // 4. When you need the final piece of information, you call run on the reader with initial data structure
  // Looks like dependency injection!
}
