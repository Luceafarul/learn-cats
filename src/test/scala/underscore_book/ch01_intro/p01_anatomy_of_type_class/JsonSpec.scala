package underscore_book.ch01_intro.p01_anatomy_of_type_class

import book.ch01_intro.p01_anatomy_of_type_class.{JsNull, JsObject, JsString, Json, Person}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class JsonSpec extends AnyFlatSpec with Matchers {
  "The Person object" should "transform to JSON" in {
    import book.ch01_intro.p01_anatomy_of_type_class.Person._

    val person = Person("Marcus", 37, "marcus@test.mail")
    val json = Json.toJson(person)
    json shouldEqual JsObject(
      Map("name" -> JsString(person.name), "email" -> JsString(person.email))
    )
  }

  it should "transform to JSON with syntax" in {
    import book.ch01_intro.p01_anatomy_of_type_class.JsonSyntax._
    import book.ch01_intro.p01_anatomy_of_type_class.Person._

    val person = Person("Marcus", 37, "marcus@test.mail")
    val json = person.toJson
    json shouldEqual JsObject(
      Map("name" -> JsString(person.name), "email" -> JsString(person.email))
    )
  }

  "The Option object" should "transform Some String to JSON String" in {
    import book.ch01_intro.p01_anatomy_of_type_class.JsonSyntax._
    import book.ch01_intro.p01_anatomy_of_type_class.Person._

    Json.toJson(Option("Hello")) shouldEqual JsString("Hello")
  }

  it should "transform None to JsNull" in {
    import book.ch01_intro.p01_anatomy_of_type_class.JsonSyntax._
    import book.ch01_intro.p01_anatomy_of_type_class.Person._

    Json.toJson(Option.empty[String]) shouldEqual JsNull
  }
}
