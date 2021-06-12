package underscore_book.ch01_intro.p02_exercise

trait SpecHelper {

  final case class Cat(name: String, age: Int, color: String)

  val cat = Cat("Marcus", 7, "Black")
  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")
}
