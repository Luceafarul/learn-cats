package blogs.monad

import scala.concurrent.Future
import scala.util.{Failure, Success}

// https://blog.rockthejvm.com/monads/
object MonadExplainByRTJVM extends App {

  // 1. The pattern
  // Monads are types which take values and do something interesting to them by following a certain structure.
  // 99% of the value of monads is in their structure. Structure of the monad is a key?
  final case class SafeValue[+T](private val value: T) {
    def get: T = synchronized { value }
  }

  // Let's assume some external API provide SafeValue
  // We transform it into another SafeValue
  val safeString: SafeValue[String] = apiThatReturnSafeValue("Wonderful string!")
  val string = safeString.get
  val upperString = string.toUpperCase
  val upperSafeString = SafeValue(upperString)
  // println(upperSafeString)

  // Pay close attention to this pattern.
  // Someone gives you a safe value, we unwrap it, transform it, then create another safe value.
  // Again:
  // - Unwrap
  // - Transform
  // - Create another value in the same context

  final case class SafeValue2[+T](private val value: T) {
    def transform[S](transformer: T => SafeValue2[S]): SafeValue2[S] = synchronized(transformer(value))
  }

  object SafeValue2 {
    def fromSafeValue[T](safeValue: SafeValue[T]): SafeValue2[T] = SafeValue2(safeValue.get)
  }

  // Now we don't need get, we just apply transform function and pass our transformer
  val safeString2: SafeValue2[String] = SafeValue2.fromSafeValue(apiThatReturnSafeValue("Another wonderful string!"))
  val upperSafeString2: SafeValue2[String] = safeString2.transform(s => SafeValue2(s.toUpperCase))
  // println(upperSafeString2)

  // This pattern (and its refactor) is very common, and is composed of two things:
  // 1. the ability to wrap a value into my (more interesting) type - in OO terms this is just a "constructor";
  //    we call this _unit_, or _pure_, or _apply_
  // 2. a function that transforms a wrapper into another wrapper (perhaps of another type) in the same style as the above
  //    we usually call this _bind_ or _flatMap_
  // When you’re dealing with this Extract-Transform-Wrap pattern, you’ve created the conditions for a monad.

  private def apiThatReturnSafeValue(s: String): SafeValue[String] = SafeValue(s)

  // 2. Examples from the jungle
  // Example 1: A census applicationPermalink
  // Assume we're populating a database with people, and have the following code:
  final case class Person(firstName: String, lastName: String) {
    require(firstName != null && lastName != null, "First and/or last names should not be null.")
  }

  def oldWeirdApiForGetPerson(firstName: String, lastName: String): Person =
    if (firstName != null) {
      if (lastName != null) Person(firstName.capitalize, lastName.capitalize) else null
    } else null

  // We can more elegantly solve this with Options.
  // Options have both structural elements of monads: a “constructor” and a flatMap.
  def getPersonWithOption(firstName: String, lastName: String): Option[Person] =
    Option(firstName).flatMap { f =>
      Option(lastName).map { l =>
        Person(f.capitalize, l.capitalize)
      }
    }

  // We can then reduce this to for-comprehensions:
  def getPersonWithFor(firstName: String, lastName: String): Option[Person] =
    for {
      f <- Option(firstName)
      l <- Option(lastName)
    } yield Person(f.capitalize, l.capitalize)

  // Example 2: asynchronous fetchesPermalink
  // Imagine we’re calling some async services for your online store, like fetching from some external resource:
  final case class User(id: String)
  final case class Product(sku: String, price: BigDecimal)

  // Some API
  import scala.concurrent.ExecutionContext.Implicits.global
  def getUser(url: String): Future[User] = Future { User("731") }
  def getLastOrder(userId: String): Future[Product] = Future { Product("0981", BigDecimal(7.23)) }

  val userFuture = getUser("my.store.com/users/daniel")

  // We want to get last order of the user

  // Bad approach
  userFuture.onComplete {
    case Success(user) =>
      val lastOrder = getLastOrder(user.id)
      lastOrder.onComplete {
        case Success(product)   => println(s"Last user's order: $product, and price with vat: ${product.price * 1.19}")
        case Failure(exception) => println(s"Failed when fetch user's last order: $exception")
      }
    case Failure(exception) => println(s"Failed when fetch user: $exception")
  }

  // Better approach
  val priceWithVat =
    getUser("my.store.com/users/daniel")
      .flatMap(user => getLastOrder(user.id))
      .map(product => product.price * 1.19)

  val priceWithVatFor =
    for {
      user <- getUser("my.store.com/users/daniel")
      product <- getLastOrder(user.id)
    } yield product.price * 1.19

  // Example 3: double-for “loops”Permalink
  // Lists follow the same pattern.
  // Here’s an example: you want to return a “checkerboard” (i.e. a cartesian product) of all possible combinations
  // of elements from two lists.
  val ns = List(1, 2, 3)
  val chs = List('a', 'b', 'c')

  val checkerboard = ns.flatMap(n => chs.map(c => n -> c))
  println(checkerboard)

  val checkerboard2 =
    for {
      n <- ns
      c <- chs
    } yield n -> c
  println(checkerboard2)

  // 3. The annoying axioms
  // Property 1: left-identity, a.k.a. the “if I had it” pattern
  // If you have the x the monad was built with, you want to apply the transformation on it, so you get f(x).
  // If you don’t, you need the ETW pattern, so you need Monad(x).flatMap(f).
  // Either way, you get the same thing. For all monads, Monad(x).flatMap(f) == f(x).

  // Property 2: left-associativity, a.k.a. the useless wrap
  // Assume you have Monad(x). What should happen when you say
  // Monad(x).flatMap(x => Monad(x))
  // If it’s not obvious yet, make it concrete:
  // example with lists: List(x).flatMap(x => List(x)) = ?
  // example with Futures: Future(x).flatMap(x => Future(x))= ?
  // Nothing changes, right? An inherent property of monads is that Monad(x).flatMap(x => Monad(x)) is the same as Monad(x).

  // Property 3: right-associativity, a.k.a. ETW-ETWPermalink
  // This one is critical, and describes the correctness of multiple flatMap chains on monads. Say you have a list and some transformations:
  // Monad(x).flatMap(f).flatMap(g) == Monad(x).flatMap(x => f(x).flatMap(g))

  val numbers = List(1, 2, 3)
  val incrementer = (x: Int) => List(x, x + 1)
  val doubler = (x: Int) => List(x, 2 * x)

  numbers.flatMap(incrementer).flatMap(doubler) == numbers.flatMap(x => incrementer(x).flatMap(doubler))
  // [1, 2, 3] => incrementer => [1, 2, 2, 3, 3, 4] => doubler => [1, 2, 2, 4, 2, 4, 3, 6, 3, 6, 4, 8]
  // [1, 2, 3]             => incrementer => doubler =>           [1, 2, 2, 4, 2, 4, 3, 6, 3, 6, 4, 8]

  println(numbers.flatMap(incrementer).flatMap(doubler))
  println(numbers.flatMap(x => incrementer(x).flatMap(doubler)))
  println(numbers.flatMap(incrementer).flatMap(doubler) == numbers.flatMap(x => incrementer(x).flatMap(doubler)))

  // 4. Epilogue
  // Monads inherently describe sequential ETW computations:
  // - extract this,
  // - then transform,
  // - then wrap.
  // If you want to process it further, same thing: extract, transform, wrap.
  // If you got this far, congratulations!
  // The gist is this: whenever you need to ETW, you probably need a monad, which needs
  //  1. a constructor,
  //  2. a flatMap
  // in the style of the above.
}
