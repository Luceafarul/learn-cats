package courses.rockthejvm.part2

object _2_Monoids extends App {

  import cats.Semigroup
  import cats.syntax.option._
  import cats.syntax.semigroup._

  // Combine |+| is always associative
  val numbers = (1 to 1000).toList
  val sumLeft = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)

  println(s"Sum left: $sumLeft")
  println(s"Sum right: $sumRight")

  // Define a general API
  def combineFoldWhat[T](xs: List[T])(implicit semigroup: Semigroup[T]): T =
    xs.fold(???)(_ |+| _)

  // Monoid, can provide a "zero"-value

  import cats.Monoid

  val intMonoid = Monoid[Int]
  println(s"Monoid int empty: ${intMonoid.empty}")
  println(s"Monoid list empty: ${Monoid[List[Throwable]].empty}")

  // Let's update a general API
  def combineFold[T](xs: List[T])(implicit monoid: Monoid[T]): T =
    xs.fold(monoid.empty)(_ |+| _)

  println(s"Combine fold: ${combineFold(List("Hello, ", "darkness ", "my ", "old ", "friend"))}")
  println(s"Combine fold: ${combineFold(numbers)}")

  val combineOption = 13.some |+| none[Int]
  println(s"Combine option: $combineOption")

  // Combine a list of phonebooks as Map[String, Int]
  val phonebooks = List(
    Map(
      "Alice" -> 123,
      "Jil" -> 321,
    ),
    Map(
      "Tom" -> 723,
      "Tina" -> 287,
    ),
    Map(
      "Tony" -> 873
    )
  )
  println(s"Combine phonebooks: ${combineFold(phonebooks)}")

  // Aggregate shopping carts
  final case class ShoppingCart(items: List[String], total: Double)

  object ShoppingCart {
    implicit val shoppingCartMonoid: Monoid[ShoppingCart] = new Monoid[ShoppingCart] {
      def empty: ShoppingCart = ShoppingCart(List.empty, 0.0)

      def combine(cartOne: ShoppingCart, cartTwo: ShoppingCart): ShoppingCart =
        ShoppingCart(cartOne.items ++ cartTwo.items, cartOne.total + cartTwo.total)
    }

    // The answer with using instance method:
    // implicit val shoppingCartMonoidAnswer: Monoid[ShoppingCart] =
    //   Monoid.instance[ShoppingCart](
    //     ShoppingCart(List.empty, 0.0),
    //     (cartOne, cartTwo) => ShoppingCart(cartOne.items ++ cartTwo.items, cartOne.total + cartTwo.total)
    //   )
  }

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart =
    combineFold(shoppingCarts)

  val shoppingCarts = List(
    ShoppingCart(List("Banana", "Orange"), 8.98),
    ShoppingCart(List("iPhone"), 899),
    ShoppingCart(List("iPad", "MacBook Pro"), 3198.00),
    ShoppingCart(List("Cheese", "Apples", "Pasta"), 37.97),
  )
  println(s"Combined shopping carts: ${checkout(shoppingCarts)}")
}
