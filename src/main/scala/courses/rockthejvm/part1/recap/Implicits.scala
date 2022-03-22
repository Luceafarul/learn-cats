package courses.rockthejvm.part1.recap

object Implicits extends App {
  // Implicits:
  // The compiler automatically searches for potential implicits if:
  //  - use a method that doesn't belong to a class
  //  - call a method with implicit arguments
  // Must have exactly one implicit of that type in scope
  // The compiler searches for implicits in:
  //  - the local scope = clearly defined implicit val/def/class
  //  - the important scope
  //  - the companion objects of the types involved in the method call

  // Implicit classes
  // - has only one argument
  final case class Person(name: String) {
    def greet: String = s"Hi, my name is $name!"
  }

  implicit class ImpersonableString(name: String) {
    def greet: String = Person(name).greet
  }

  // Explicit way (what compiler do under the hood):
  val explicitImpersonableString = new ImpersonableString("Tom")
  explicitImpersonableString.greet

  // Implicit:
  val tomGreeting = "Tom".greet
  println(tomGreeting)

  // Importing implicit conversions in scope

  import scala.concurrent.duration._

  val oneSec = 1.second
  println(oneSec)

  // Implicit arguments and values
  // Implicit argument is used to PROVE THE EXISTENCE of a type.
  def increment(x: Int)(implicit amount: Int): Int = x + amount

  implicit val defaultImplicitInt: Int = 10

  println(s"Increment $defaultImplicitInt by 2: ${increment(2)}")

  def multiply(x: Int)(implicit times: Int): Int = x * times

  println(s"Multiply $defaultImplicitInt by 2: ${multiply(2)}")

  // More complex example
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  def listToJson[T](xs: List[T])(implicit serializer: JSONSerializer[T]): String =
    xs.map(serializer.toJson).mkString("[", ",", "]")

  implicit val personSerializer: JSONSerializer[Person] = new JSONSerializer[Person] {
    override def toJson(p: Person): String =
      s"""
         |{"name" : "${p.name}"}
         |""".stripMargin.trim
  }

  println(s"JSON Persons: ${listToJson(List(Person("Tommy"), Person("Anna")))}")

  // Implicit methods
  // Implicit methods are used to PROVE THE EXISTENCE of a type.
  // Can be used for implicit conversions (DISCOURAGED)
  implicit def oneArgCaseClassSerializer[T <: Product]: JSONSerializer[T] = new JSONSerializer[T] {
    override def toJson(value: T): String =
      s"""
         |{"${value.productElementName(0)}" : "${value.productElement(0)}"}
         |""".stripMargin.trim
  }

  final case class Cat(catName: String)

  println(s"JSON Cats: ${listToJson(List(Cat("Garfield"), Cat("Cassiopeia")))}")
  println(s"oneArgCaseClassSerializer: ${oneArgCaseClassSerializer[Cat].toJson(Cat("Tom"))}")
}
