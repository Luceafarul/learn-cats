package underscore_book.ch01_intro.p02_exercise

import cats.Show
import cats.syntax.show._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ShowSpec extends AnyFlatSpec with Matchers with SpecHelper {

  private val result = "Marcus is a 7 year-old black cat."

  "Show" should "return String representation of Cat" in {
    implicit val showCat: Show[Cat] = (cat: Cat) => {
      s"${cat.name} is a ${cat.age} year-old ${cat.color.toLowerCase} cat."
    }

    cat.show shouldBe result
  }

  it should "return String representation of Cat (using show method)" in {
    implicit val showCat: Show[Cat] = Show.show { cat =>
      s"${cat.name} is a ${cat.age} year-old ${cat.color.toLowerCase} cat."
    }

    cat.show shouldBe result
  }
}
