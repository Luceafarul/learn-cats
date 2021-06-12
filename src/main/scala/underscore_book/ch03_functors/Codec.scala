package underscore_book.ch03_functors

trait Codec[A] { self =>

  def encode(value: A): String
  def decode(s: String): A

  def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
    def encode(value: B): String = self.encode(enc(value))
    def decode(s: String): B = dec(self.decode(s))
  }
}

object Codec {
  def encode[A](value: A)(implicit codec: Codec[A]): String = codec.encode(value)

  def decode[A](s: String)(implicit codec: Codec[A]): A = codec.decode(s)

  implicit val stringCodec: Codec[String] = new Codec[String] {
    def encode(value: String): String = value
    def decode(s: String): String = s
  }

  implicit val intCodec: Codec[Int] = stringCodec.imap(s => s.toInt, i => i.toString)

  implicit val doubleCodec: Codec[Double] = stringCodec.imap(s => s.toDouble, d => d.toString)

  implicit val booleanCodec: Codec[Boolean] = stringCodec.imap(
    s => if (s.equalsIgnoreCase("yes")) true else false,
    b => if (b) "yes" else "no"
  )

  implicit def boxCodec[A](implicit codec: Codec[A]): Codec[Box[A]] = codec.imap(
    s => Box(s),
    box => box.value
  )
}
