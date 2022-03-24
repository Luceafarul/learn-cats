package book.ch01_intro.p01_anatomy_of_type_class

final case class Person(name: String, age: Int, email: String)

object Person {
  implicit val stringWriter: JsonWriter[String] = (value: String) => JsString(value)

  implicit val personWriter: JsonWriter[Person] = (value: Person) => JsObject(Map(
    "name" -> JsString(value.name),
    "email" -> JsString(value.email)
  ))
}
