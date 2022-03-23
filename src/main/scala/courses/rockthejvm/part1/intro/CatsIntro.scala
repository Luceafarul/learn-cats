package courses.rockthejvm.part1.intro

object CatsIntro extends App {
  // Eq
  // In Scala 3 this comparison 2 == "string" is not possible
  val aComparison = 2 == "string"
  println(aComparison)

  // Step 1. Import type class - cats.Eq
  import cats.Eq

  // Step 2. Import type class instances for the types you need
  import cats.instances.int._

  // Step 3. Use type class API
  val intEquality = Eq[Int]
  val aTypeSafeComparison = intEquality.eqv(3, 4) // Doesn't compile with other types
  println(aTypeSafeComparison)

  // Step 4. Use extension methods (if applicable)
  import cats.syntax.eq._
  val anotherTypeSafeComparison = 3 === 3
  println(anotherTypeSafeComparison)
  println(2 =!= 3)
  // println(2 =!= "string") // DOESN'T compile

  // Step 5. Extending the type class operations to composite types (like List)
  // Extension method are only visible in the presence of the right type class instance
  import cats.instances.list._ // bring Eq[List[Int]] in scope
  val aListComparison = List(1, 2) === List(3, 4)
  println(aListComparison)

  // Step 6. Create a type class instance for a custom type
  final case class ToyCar(model: String, price: Double)
  object ToyCar {
    implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] { (car1, car2) =>
      car1.model.equalsIgnoreCase(car2.model) && car1.price === car2.price
    }
  }

  import ToyCar._
  val tc1 = ToyCar("bmw", 1.99)
  val tc2 = ToyCar("BMW", 1.99)
  val tc3 = ToyCar("BMW", 1.98)

  println(s"tc1 == tc2 ? => ${tc1 === tc2}")
  println(s"tc2 == tc3 ? => ${tc2 === tc3}")

  // Conclusion
  // Cats organization:
  // Most important functionalities are type classes
  // - use type class API                         import cats.TYPE_CLASS
  // - bring implicit type class instances for    import cats.instances.TYPE_CLASS
  //   your supported type in scope
  // - use extension methods type class supports  import cats.syntax.TYPE_CLASS

  // Some imports are not self-evident, import all:
  // - import cats._
  // - import cats.implicits._
}
