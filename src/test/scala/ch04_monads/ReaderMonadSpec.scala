package ch04_monads

import org.scalatest.{WordSpec, Matchers}

class ReaderMonadSpec extends WordSpec with Matchers {
  "ReaderMonad" should {
    import ReaderMonad._
    val users = Map(1 -> "dade", 2 -> "kate", 3 -> "margo")
    val passwords = Map("dade" -> "zerocool", "kate" -> "acidburn", "margo" -> "secret")
    val db = Db(users, passwords)

    "return true if login exist and password is correct" in {
      val result = checkLogin(2, "acidburn").run(db)
      result shouldBe true
    }

    "return false if login exist but password is wrong" in {
      val result = checkLogin(3, "wrong").run(db)
      result shouldBe false
    }

    "return false if does not exist" in {
      val result = checkLogin(7, "secret").run(db)
      result shouldBe false
    }
  }
}
