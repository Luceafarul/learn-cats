package underscore_book.ch01_intro.p02_exercise

object PrintableInstances {
  implicit val printableString: Printable[String] = (value: String) => value

  implicit val printableInt: Printable[Int] = (value: Int) => value.toString

  implicit val printableBoolean: Printable[Boolean] = (value: Boolean) => if (value) "yes" else "no"
}
