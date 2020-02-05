package ch01_intro.p02_exercise

import ch01_intro.p01_anatomy_of_type_class.Person
import org.scalatest.{FlatSpec, Matchers}

class PrintableSpec extends FlatSpec with Matchers {

  import ch01_intro.p02_exercise.PrintableSyntax._
  import ch01_intro.p02_exercise.PrintableInstances._

  "Printable format" should "accept Int and return String representation" in {
    Printable.format(173) shouldBe "173"
    173.format shouldBe "173"
  }

  it should "accept Person and return String representation" in {
    implicit val printablePerson: Printable[Person] = (person: Person) => s"Person(${person.name}, ${person.email})"

    val marcus = Person("Marcus", "marcus@test.mail")
    Printable.format(marcus) shouldBe "Person(Marcus, marcus@test.mail)"
    marcus.format shouldBe "Person(Marcus, marcus@test.mail)"
  }

  it should "accept Cat and return String representation" in {
    final case class Cat(name: String, age: Int, color: String)

    implicit val printablePerson: Printable[Cat] = (cat: Cat) => {
      val name = Printable.format(cat.name)
      val age = Printable.format(cat.age)
      val color = Printable.format(cat.color)
      s"$name is a $age year-old ${color.toLowerCase} cat."
    }

    val marcus = Cat("Marcus", 7, "Black")
    Printable.format(marcus) shouldBe "Marcus is a 7 year-old black cat."
    marcus.format shouldBe "Marcus is a 7 year-old black cat."
  }
}
