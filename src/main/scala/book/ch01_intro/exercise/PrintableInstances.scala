package book.ch01_intro.exercise

object PrintableInstances {
  implicit val printableString: Printable[String] = (value: String) => value

  implicit val printableInt: Printable[Int] = (value: Int) => value.toString

  implicit val printableBoolean: Printable[Boolean] = (value: Boolean) => if (value) "yes" else "no"
}
