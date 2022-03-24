package underscore_book.ch01_intro.p01_anatomy_of_type_class

import book.ch01_intro.p01_anatomy_of_type_class.{JsNull, JsNumber, JsObject, JsString, Json, Person}
import book.ch01_intro.p01_anatomy_of_type_class.Person._
import book.ch01_intro.p01_anatomy_of_type_class.JsonSyntax._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class JsonSpec extends AnyFlatSpec with Matchers {
  "The Person object" should "transform to JSON" in {
    val person = Person("Marcus", 37, "marcus@test.mail")
    val result = JsObject(
      Map(
        "name" -> JsString(person.name),
        "age" -> JsNumber(37),
        "email" -> JsString(person.email))
    )

    Json.toJson(person) shouldEqual result
    person.toJson shouldEqual result
  }

  "The Option object" should "transform Some String to JSON String" in {
    Json.toJson(Option("Hello")) shouldEqual JsString("Hello")
  }

  it should "transform None to JsNull" in {
    Json.toJson(Option.empty[String]) shouldEqual JsNull
  }
}
