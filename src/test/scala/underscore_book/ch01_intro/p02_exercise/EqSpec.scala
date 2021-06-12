package underscore_book.ch01_intro.p02_exercise

import cats.Eq
import cats.syntax.eq._
import cats.instances.int._
import cats.instances.string._
import cats.instances.option._
import org.scalatest.{FlatSpec, Matchers}

class EqSpec extends FlatSpec with Matchers with SpecHelper {

  "Eq[Cat]" should "return equality and inequality of two instances" in {
    implicit val catEq: Eq[Cat] = (cat1, cat2) => {
      (cat1.name eqv cat2.name) &&
        (cat1.age eqv cat2.age) &&
        (cat1.color eqv cat2.color)
    }

    val optionCat1 = Option(cat1)
    val optionCat2 = Option.empty[Cat]

    cat1 eqv cat2 shouldBe false
    cat1 eqv cat1 shouldBe true
    cat1 neqv cat2 shouldBe true
    optionCat1 =!= optionCat2 shouldBe true
    optionCat1 eqv optionCat1 shouldBe true
  }
}
