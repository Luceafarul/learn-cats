package book.ch01_intro.p01_anatomy_of_type_class

final case class Person(name: String, age: Int, email: String)

object Person {

  import JsonSyntax._

  implicit val personWriter: JsonWriter[Person] = (p: Person) => JsObject(Map(
    "name" -> p.name.toJson,
    "age" -> p.age.toJson,
    "email" -> p.email.toJson
  ))
}
