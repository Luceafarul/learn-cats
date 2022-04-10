package book.ch04_monads.examples

import cats.data.Reader
import cats.implicits.catsSyntaxApplicativeId

object ReaderMonad extends App {
  final case class Cat(name: String, favoriteFood: String)

  val catName: Reader[Cat, String] = Reader(cat => cat.name)

  val garfield = Cat("Garfield", "lasagne")
  println(s"Cat name: ${catName.run(garfield)}")

  val greetKitty: Reader[Cat, String] = catName.map(name => s"Hello $name")

  val heathcliff = Cat("Heathcliff", "junk food")
  println(s"Greet kitty: ${greetKitty.run(heathcliff)}")

  val feedKitty: Reader[Cat, String] = Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

  val greetAndFeed: Reader[Cat, String] =
    for {
      greet <- greetKitty
      feed <- feedKitty
    } yield s"$greet. $feed."
  println(s"Greet and feed cat: ${greetAndFeed.run(garfield)}")
  println(s"Greet and feed cat: ${greetAndFeed.run(heathcliff)}")

  // Exercise:
  final case class DB(usernames: Map[Int, String], passwords: Map[String, String])

  type DBReader[T] = Reader[DB, T]

  def findUsername(userId: Int): DBReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DBReader[Boolean] =
    Reader(db => db.passwords.exists { case (k, v) =>
      k.equalsIgnoreCase(username) && v.equals(password)
    })

  def checkPasswordAnswer(username: String, password: String): DBReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DBReader[Boolean] =
    findUsername(userId).flatMap {
      case None => false.pure[DBReader]
      case Some(user) => checkPassword(user, password)
    }

  def checkLoginFor(userId: Int, password: String): DBReader[Boolean] =
    for {
      username <- findUsername(userId)
      login <- username.map { user =>
        checkPassword(user, password)
      }.getOrElse(false.pure[DBReader])
    } yield login

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )

  val db = DB(users, passwords)

  val zerocool = checkLogin(1, "zerocool").run(db)
  val davinci = checkLogin(4, "davinci").run(db)
  println(s"Check login for 'zerocool': $zerocool")
  println(s"Check login for 'zerocool': $davinci")
}
