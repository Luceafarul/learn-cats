package ch01_intro.p02_exercise

trait Printable[A] {
  def format(value: A): String

  def contramap[B](f: B => A): Printable[B] = new Printable[B] {
    def format(value: B): String = value.toString
  }
}

object Printable {
  def format[A](value: A)(implicit printable: Printable[A]): String = printable.format(value)

  def print[A](value: A)(implicit printable: Printable[A]): Unit = println(format(value))
}
