package underscore_book.ch04_monads

import org.scalatest.{WordSpec, Matchers}

class OptionMonadSpec extends WordSpec with Matchers {

  final case class Person(name: String, age: Int)

  "OptionMonad" should {
    import book.ch04_monads.OptionMonad._

    "wrap value with pure method" in {
      val person = Person("Marcus", 73)
      val result = optionMonad.pure(person)

      result shouldBe Some(person)
    }

    "transfrom content with flatMap" in {
      val personOption: Option[Person] = Some(Person("Marcus", 73))
      val result = optionMonad.flatMap(personOption)(p => Some(p.name))

      result shouldBe Some("Marcus")
    }
  }
}
