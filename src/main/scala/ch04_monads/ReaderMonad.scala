package ch04_monads

import cats.data.Reader
import cats.syntax.applicative._

object ReaderMonad {
  final case class Db(
      usernames: Map[Int, String],
      passwords: Map[String, String]
  )

  type DbReader[A] = Reader[Db, A]

  // DbReader[Option[String]] == Reader[Db, Option[String]]
  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).getOrElse(false) == password)

  def checkPasswordSolution(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      username <- findUsername(userId)
      isCorrect <- username
        .map { name => checkPassword(name, password) }
        .getOrElse { false.pure[DbReader] }
    } yield isCorrect
}
