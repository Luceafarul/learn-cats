package cats_docs.doc03_functor

import cats.Functor
import cats.implicits._

object FunctorExample extends App {
  // Functor is a type class that abstracts over type constructors that can be map‘ed over.
  // Examples of such type constructors are List, Option, and Future.

  // In => * is the same as Function1[In, *]
  implicit def function1Functor[In]: Functor[In => *] = new Functor[In => *] {
    override def map[A, B](fa: In => A)(f: A => B): In => B = fa andThen f
  }

  // map
  val res1 =  Functor[List].map(List("hello", "darkness", "my"))(_.length)
  println(res1)

  val res2 = Functor[Option].map(Option("Hello"))(_.length)
  println(res2)

  val res3 = Functor[Option].map(None: Option[String])(_.length)
  println(res3)

  // lift
  val opt1 = Option(42)
  val res4 = Functor[Option].lift((x: Int) => x + 10)(opt1)
  println(res4)

  val len: String => Int = _.length
  val lenOption: Option[String] => Option[Int] = Functor[Option].lift(len)
  val res5 = lenOption(Some("hello"))
  println(res5)

  // fproduct
  val res6 = Functor[Option].fproduct(Option(42))(_.toString)
  println(res6)

  val source = List("Cats", "is", "awesome")
  val product = Functor[List].fproduct(source)(_.length)
  println(product)

  // compose
  val listOpt = Functor[List] compose Functor[Option]
  val res7 = listOpt.map(List(Some(1), None, Some(3)))(_ + 1)
  println(res7)
}
