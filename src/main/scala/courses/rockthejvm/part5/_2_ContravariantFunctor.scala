package courses.rockthejvm.part5

object _2_ContravariantFunctor extends App {

  trait Format[T] { self =>
    def format(value: T): String

    def contramap[A](f: A => T): Format[A] = new Format[A] {
      def format(value: A): String = self.format(f(value))
    }
  }

  def format[A](value: A)(implicit F: Format[A]): String = F.format(value)

  implicit object StringFormat extends Format[String] {
    def format(value: String): String = s"\"$value\""
  }

  implicit object IntFormat extends Format[Int] {
    def format(value: Int): String = s"Int: $value"
  }

  implicit object BooleanFormat extends Format[Boolean] {
    def format(value: Boolean): String = if (value) "Y" else "N"
  }

  println("Using format:")
  println(format("Hello darkness my old friend!"))
  println(format(73))
  println(format(true))
  println(format(Option(73)))
  // println(format[Option[Int]](None)) -> failed
  println()

  // Problem:
  // - given Format[CustomType], can we have a Format[Option[CustomType]], or Format[List[...]]

  // We don't need methods below more, because we have contramap in Format
  // implicit def optionFormat[T](implicit F: Format[T]): Format[Option[T]] = new Format[Option[T]] {
  //   def format(value: Option[T]): String = F.format(value.get)
  // }

  // def contramap[A, T](f: A => T)(implicit F: Format[T]): Format[A] = new Format[A] {
  //   def format(value: A): String = F.format(f(value))
  // }

  implicit def optionFormatContramap[T](implicit F: Format[T]): Format[Option[T]] =
    F.contramap[Option[T]](_.get)

  // IntFormat:
  // fo1: Format[Option[Int]] = IntFormat.contramap[Option[Int]](_.get) // First get
  // fo2: Format[Option[Option[Int]]] = fo1.contramap[Option[Option[Int]]](_.get) // Second get
  //
  // fo2 = IntFormat
  //         .contramap[Option[Int]](_.get) // First get
  //         .contramap[Option[Option[Int]]](_.get) // Second get
  //
  // fo2.format(Option(Option(73)) =
  //   fo1.format(secondGet(Option((Option(73))) =
  //   IntFormat.format(firstGet(secondGet(Option((Option(73))))
  //
  // Order = REVERSE from the written order:
  // - second get
  // - first get
  // - format of Int
  //
  // Map applies transformations in sequence
  // Contramap applies transformations in REVERSE sequence

  import cats.Contravariant
  import cats.Show

  val showInts = Show[Int]
  val showOption: Show[Option[Int]] = Contravariant[Show].contramap(showInts)(_.getOrElse(0))

  import cats.syntax.contravariant._
  val showOptionShorter: Show[Option[Int]] = showInts.contramap(_.getOrElse(0))

  println(showOptionShorter.show(Some(13)))
}
