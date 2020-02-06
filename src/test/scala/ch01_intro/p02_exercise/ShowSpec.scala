package ch01_intro.p02_exercise

import cats.Show
import org.scalatest.{FlatSpec, Matchers}

class ShowSpec extends FlatSpec with Matchers with SpecHelper {

  import cats.syntax.show._

  "Show" should "return String representation of Cat" in {
    implicit val showCat: Show[Cat] = (cat: Cat) => {
      import cats.instances.int._
      import cats.instances.string._

      val name = cat.name.show
      val age = cat.age.show
      val color = cat.color.show.toLowerCase

      s"$name is a $age year-old $color cat."
    }

    cat.show shouldBe "Marcus is a 7 year-old black cat."
  }
}
