package courses.rockthejvm.part5

import cats.Monoid

object _3_InvariantFunctor extends App {

  trait Crypto[A] { self =>
    def encrypt(value: A): String
    def decrypt(encrypted: String): A

    def imap[B](back: B => A, forth: A => B): Crypto[B] = new Crypto[B] {
      def encrypt(value: B): String = self.encrypt(back(value))
      def decrypt(encrypted: String): B = forth(self.decrypt(encrypted))
    }
  }

  def encrypt[A](value: A)(implicit C: Crypto[A]): String = C.encrypt(value)
  def decrypt[A](encrypted: String)(implicit C: Crypto[A]): A = C.decrypt(encrypted)

  implicit val caesarCypher: Crypto[String] = new Crypto[String] {
    def encrypt(value: String): String = value.map(c => (c + 2).toChar)
    def decrypt(value: String): String = value.map(c => (c - 2).toChar)
  }

  val encrypted: String = encrypt("Let's encrypt it!")
  val decrypted: String = decrypt[String](encrypted)

  println(encrypted)
  println(decrypted)

  // How can we support caesar cypher logic for other types like Int, Double, Option[String]?
  implicit val doubleCrypto: Crypto[Double] = caesarCypher.imap(_.toString, _.toDouble)

  println(encrypt(Math.PI))
  println(decrypt[Double](encrypt(Math.PI)))

  // Exercise: support Option[String]
  implicit val optionStringCrypto: Crypto[Option[String]] = caesarCypher.imap(_.getOrElse(""), Option(_))

  val encryptedOption: String = encrypt(Option("Let's encrypt it!"))
  val decryptedOption: Option[String] = decrypt[Option[String]](encryptedOption)
  println(encryptedOption)
  println(decryptedOption)

  // Exercise: If you have a Crypto[T] you can derived a Crypto[Option[T]] if you have Monoid[T] in scope
  implicit def optionCrypto[T](implicit C: Crypto[T], M: Monoid[T]): Crypto[Option[T]] =
    C.imap(_.getOrElse(M.empty), Option(_))

  println(encrypt(Option(Math.PI)))
  println(decrypt[Option[Double]](encrypt(Math.PI)))

  import cats.Invariant
  import cats.Show

  val showString = Show[String]
  val showOptionString = Invariant[Show].imap(showString)(Option(_))(_.getOrElse(""))

  import cats.syntax.invariant._
  val showOptionStringShorter = showString.imap(Option(_))(_.getOrElse(""))

  // Exercise: what's the relationship?
  trait _Invariant[F[_]] {
    def imap[A, B](fa: F[A])(forth: A => B)(back: B => A): F[B]
  }

  trait _Contravariant[F[_]] extends _Invariant[F] {
    def contramap[A, B](fa: F[A])(back: B => A): F[B]

    def imap[A, B](fa: F[A])(forth: A => B)(back: B => A): F[B] =
      contramap(fa)(back)
  }

  trait _Functor[F[_]] extends _Invariant[F] {
    def map[A, B](fa: F[A])(forth: A => B): F[B]

    def imap[A, B](fa: F[A])(forth: A => B)(back: B => A): F[B] =
      map(fa)(forth)
  }
}
