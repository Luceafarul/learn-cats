package book.ch01_intro.exercise

trait Printable[A] { self =>

  def format(value: A): String

  def contramap[B](f: B => A): Printable[B] = new Printable[B] {
    def format(value: B): String = self.format(f(value))
  }
}

object Printable {
  def format[A](value: A)(implicit printable: Printable[A]): String =
    printable.format(value)

  def print[A](value: A)(implicit printable: Printable[A]): Unit =
    println(format(value))
}
