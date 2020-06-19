package ch01_intro.p02_exercise

import ch01_intro.p01_anatomy_of_type_class.Person
import ch03_functors._
import org.scalatest.{FlatSpec, Matchers}

class PrintableSpec extends FlatSpec with Matchers with SpecHelper {

  import ch01_intro.p02_exercise.PrintableSyntax._
  import ch01_intro.p02_exercise.PrintableInstances._

  "Printable format" should "accept Int and return String representation" in {
    Printable.format(173) shouldBe "173"
    173.format shouldBe "173"
  }

  it should "accept Person and return String representation" in {
    implicit val printablePerson: Printable[Person] =
      (person: Person) => s"Person(${person.name}, ${person.email})"

    val marcus = Person("Marcus", "marcus@test.mail")
    Printable.format(marcus) shouldBe "Person(Marcus, marcus@test.mail)"
    marcus.format shouldBe "Person(Marcus, marcus@test.mail)"
  }

  it should "accept Cat and return String representation" in {
    implicit val printablePerson: Printable[Cat] = (cat: Cat) => {
      val name = Printable.format(cat.name)
      val age = Printable.format(cat.age)
      val color = Printable.format(cat.color)
      s"$name is a $age year-old ${color.toLowerCase} cat."
    }

    Printable.format(cat) shouldBe "Marcus is a 7 year-old black cat."
    cat.format shouldBe "Marcus is a 7 year-old black cat."
  }

  it should "accept Boolean and return String representation" in {
    val t = true
    val f = false

    t.format shouldBe "yes"
    f.format shouldBe "no"
  }

  it should "return String representation of the Box with Int" in {
    val boxWithInt = Box(123)

    boxWithInt.format shouldBe "123"
  }

    it should "return String representation of the Box with True" in {
    val boxWithTrue = Box(true)

    boxWithTrue.format shouldBe "yes"
  }
}
