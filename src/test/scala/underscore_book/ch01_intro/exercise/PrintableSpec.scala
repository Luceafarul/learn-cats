package underscore_book.ch01_intro.exercise

import book.ch01_intro.anatomy_of_type_class.Person
import book.ch01_intro.exercise.Printable
import book.ch03_functors.Box
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PrintableSpec extends AnyFlatSpec with Matchers with SpecHelper {

  import book.ch01_intro.exercise.PrintableSyntax._
  import book.ch01_intro.exercise.PrintableInstances._

  "Printable format" should "accept Int and return String representation" in {
    Printable.format(173) shouldBe "173"
    173.format shouldBe "173"
  }

  it should "accept Person and return String representation" in {
    implicit val printablePerson: Printable[Person] =
      (person: Person) => s"Person(${person.name}, ${person.email})"

    val marcus = Person("Marcus", 37, "marcus@test.mail")
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
    true.format shouldBe "yes"
    false.format shouldBe "no"
  }

  it should "return String representation of the Box with Int" in {
    Box(123).format shouldBe "123"
  }

    it should "return String representation of the Box with True" in {
    Box(true).format shouldBe "yes"
  }
}
