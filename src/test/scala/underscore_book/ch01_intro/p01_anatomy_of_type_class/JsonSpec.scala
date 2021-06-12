package underscore_book.ch01_intro.p01_anatomy_of_type_class

import org.scalatest._

class JsonSpec extends FlatSpec with Matchers {
  "The Person object" should "transform to JSON" in {
    import underscore_book.ch01_intro.p01_anatomy_of_type_class.JsonWriterInstances._

    val person = Person("Marcus", "marcus@test.mail")
    val json = Json.toJson(person)
    json shouldEqual JsObject(
      Map("name" -> JsString(person.name), "email" -> JsString(person.email))
    )
  }

  it should "transform to JSON with syntax" in {
    import underscore_book.ch01_intro.p01_anatomy_of_type_class.JsonSyntax._
    import underscore_book.ch01_intro.p01_anatomy_of_type_class.JsonWriterInstances._

    val person = Person("Marcus", "marcus@test.mail")
    val json = person.toJson
    json shouldEqual JsObject(
      Map("name" -> JsString(person.name), "email" -> JsString(person.email))
    )
  }

  "The Option object" should "transform Some String to JSON String" in {
    import underscore_book.ch01_intro.p01_anatomy_of_type_class.JsonSyntax._
    import underscore_book.ch01_intro.p01_anatomy_of_type_class.JsonWriterInstances._

    Json.toJson(Option("Hello")) shouldEqual JsString("Hello")
  }

  it should "transform None to JsNull" in {
    import underscore_book.ch01_intro.p01_anatomy_of_type_class.JsonSyntax._
    import underscore_book.ch01_intro.p01_anatomy_of_type_class.JsonWriterInstances._

    Json.toJson(Option.empty[String]) shouldEqual JsNull
  }
}
