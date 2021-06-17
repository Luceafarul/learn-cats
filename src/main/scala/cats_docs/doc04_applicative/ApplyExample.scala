package cats_docs.doc04_applicative

import cats.Apply

object ApplyExample extends App {
  implicit val optionApply: Apply[Option] = new Apply[Option] {
    override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] =
      fa.flatMap(a => ff.map(f => f(a)))

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa.map(f)
  }

  implicit val listApply: Apply[List] = new Apply[List] {
    override def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] =
      fa.flatMap(a => ff.map(f => f(a)))

    override def map[A, B](fa: List[A])(f: A => B): List[B] =
      fa.map(f)
  }

  // map
  val intToString: Int => String = _.toString
  val double: Int => Int = _ * 2
  val addTwo: Int => Int = _ + 2

  println(Apply[Option].map(Some(3))(intToString))
  println(Apply[Option].map(Some(2))(double))
  println(Apply[Option].map(None)(addTwo))

  // compose
  val listOpt = Apply[List] compose Apply[Option]
  val plusOne = (x: Int) => x + 1

  println(listOpt.ap(List(Some(plusOne)))(List(Some(1), None, Some(2))))

  // ap
  println(Apply[Option].ap(Some(intToString))(Some(73)))
  println(Apply[Option].ap(Some(double))(Some(73)))
  println(Apply[Option].ap(Some(double))(None))
  println(Apply[Option].ap(None)(Some(73)))
  println(Apply[Option].ap(None)(None))

  // ap2, ap3, etc
  val addArity2 = (a: Int, b: Int) => a + b
  println(Apply[Option].ap2(Some(addArity2))(Some(73), Some(42)))
  println(Apply[Option].ap2(Some(addArity2))(Some(73), None))

  val addArity3 = (a: Int, b: Int, c: Int) => a + b + c
  println(Apply[Option].ap3(Some(addArity3))(Some(1), Some(2), Some(3)))

  // map2, map3, etc
  println(Apply[Option].map2(Some(73), Some(42))(addArity2))
  println(Apply[Option].map3(Some(1), Some(2), Some(3))(addArity3))

  // tuple2, tuple3, etc
  println(Apply[Option].tuple2(Some(73), Some(42)))
  println(Apply[Option].tuple3(Some(73), Some(42), None))

  // apply builder syntax
  import cats.implicits._

  val option2 = (Option(1), Option(2))
  val (one, two) = option2
  val option3 = (one, two, Option.empty[Int])

  println(option2 mapN addArity2)
  println(option3 mapN addArity3)

  println(option2 apWith Some(addArity2))
  println(option3 apWith Some(addArity3))

  println(option2.tupled)
  println(option3.tupled)
}
