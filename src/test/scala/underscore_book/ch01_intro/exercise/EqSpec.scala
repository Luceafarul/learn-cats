package underscore_book.ch01_intro.exercise

import cats.Eq
import cats.syntax.eq._
import cats.syntax.option._
import cats.instances.int._
import cats.instances.string._
import cats.instances.option._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EqSpec extends AnyFlatSpec with Matchers with SpecHelper {

  "Eq[Cat]" should "return equality and inequality of two instances" in {
    implicit val catEq: Eq[Cat] = Eq.instance[Cat] { (cat1, cat2) =>
      (cat1.name eqv cat2.name) &&
        (cat1.age eqv cat2.age) &&
        (cat1.color eqv cat2.color)

    }

    val optionCat1 = Option(cat1)
    val optionCat2 = Option.empty[Cat]
    val optionCat3 = cat3.some

    cat1 eqv cat2 shouldBe false
    cat1 eqv cat1 shouldBe true
    cat1 eqv cat3 shouldBe true
    cat1 =!= cat2 shouldBe true

    optionCat1 =!= optionCat2 shouldBe true
    optionCat1 eqv optionCat1 shouldBe true
    optionCat1 eqv optionCat3 shouldBe true
  }
}
