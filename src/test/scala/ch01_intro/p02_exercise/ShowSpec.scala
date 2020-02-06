package ch01_intro.p02_exercise

import cats.Show
import org.scalatest.{FlatSpec, Matchers}

class ShowSpec extends FlatSpec with Matchers {
  import cats.syntax.show._

  final case class Cat(name: String, age: Int, color: String)

  "Show" should "return String representation of Cat" in {
    val cat = Cat("Marcus", 7, "Black")

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
