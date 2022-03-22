package courses.rockthejvm.part1.recap

object TypeClasses extends App {
  final case class Person(name: String, age: Int)

  // Step 1. Type class definition
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  // Step 2. Create implicit type class Instances
  implicit object StringSerializer extends JSONSerializer[String] {
    def toJson(value: String): String = s"\"$value\""
  }

  implicit object IntSerializer extends JSONSerializer[Int] {
    def toJson(value: Int): String = value.toString
  }

  implicit object PersonSerializer extends JSONSerializer[Person] {
    def toJson(value: Person): String =
      s"""
         |{"name" : ${value.name}, "age" : ${value.age}}
         |""".stripMargin.trim
  }

  // Step 3. Offer some API
  def listToJSON[T](xs: List[T])(implicit serializer: JSONSerializer[T]): String =
    xs.map(serializer.toJson).mkString("[", ",", "]")

  // Step 4. Extending the existing types via extension methods
  object JSONSyntax {
    implicit class JSONSerializable[T](value: T)(implicit serializer: JSONSerializer[T]) {
      def toJson: String = serializer.toJson(value)
    }
  }

  println(listToJSON(List(Person("Anna", 21), Person("Jannie", 23))))

  val bob = Person("Tommy", 37)

  import JSONSyntax._

  println(bob.toJson) // NOT compile without import
}
