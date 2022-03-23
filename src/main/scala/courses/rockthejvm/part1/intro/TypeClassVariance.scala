package courses.rockthejvm.part1.intro

object TypeClassVariance extends App {

  import cats.Eq
  import cats.instances.int._
  import cats.instances.option._
  import cats.syntax.eq._

  val aComparison = Option(2) === Option(3)
  // value === is not a member of Some[Int]
  // val anInvalidComparison = Some(2) === None // Eq[Some[Int]] not found
  Option(2) === Option.empty[Int]

  // Variance:
  class Animal
  class Cat extends Animal

  // Covariant type - subtyping is propagated to the generic type
  class Cage[+T]
  val cage: Cage[Animal] = new Cage[Cat] // Cat <: Animal, so Cage[Cat] <: Cage[Animal]

  // Contravariant type - subtyping is propagated BACKWARDS to the generic type
  class Vet[-T]
  val vet: Vet[Cat] = new Vet[Animal] // Cat <: Animal, then Vet[Animal] <: Vet[Cat]

  // Rule of thumb: "HAS a T" = covariant, "ACTS on T" = contravariant
  // Variance affected how the type classes instances are being fetched

  // Contravariant Type Class
  trait SoundMaker[-T]
  implicit object AnimalSoundMaker extends SoundMaker[Animal]

  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("wow!")
  makeSound[Animal] // OK - Type Class instance defined above
  makeSound[Cat]    // OK - Type Class instance for Animal is also applicable to Cat

  // RULE 1. Contravariant Type Classes can use the superclass instances if nothing
  //         is available strictly for that type

  // Has implications for subtypes
  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]
  makeSound[Option[Int]]
  makeSound[Some[Int]]

  // Covariant Type Class
  trait AnimalShow[+T] {
    def show: String
  }
  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    def show: String = "animals everywhere"
  }
  implicit object CatsShow extends AnimalShow[Cat] {
    def show: String = "so many cats!"
  }

  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show

  // RULE 2. Covariant Type Classes will always use the more specific Type Class instance
  //         for that type, but may confuse the compiler ig the general Type Class is also present.

  // RULE 3. You can't have both benefits. Cats uses INVARIANT Type Classes.

  println(organizeShow[Cat])        // OK - the compiler will inject CatsShow as implicit
  // println(organizeShow[Animal])  // NOT OK - do not compile: ambiguous implicit values
}
