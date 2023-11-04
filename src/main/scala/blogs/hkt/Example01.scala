package blogs.hkt

import scala.collection.immutable

object Example01 {
  // Blog: https://www.baeldung.com/scala/higher-kinded-types

  trait Collection[T[_]] {
    def wrap[A](a: A): T[A]
    def first[A](xs: T[A]): A
  }

  val listCollection =
    new Collection[List] {
      def wrap[A](a: A): List[A] = List(a)
      def first[A](xs: List[A]): A = xs.head
    }

  trait BatchRun[M[_]] {
    def write[A](item: A, database: M[A]): M[A] = transform(item, database)

    def write[A](items: List[A], database: M[A]): M[A] =
      items match {
        case Nil          => database
        case head :: tail => write(tail, transform(head, database))
      }

    def transform[A](item: A, database: M[A]): M[A]
  }

  val listDatabase: List[String] = List("Item 1", "Item 2")

  val listBatchRun = new BatchRun[List] {

    def transform[A](item: A, database: List[A]): List[A] =
      database :+ item
  }

  val saveItem = listBatchRun.write("Item 3", listDatabase)
  val saveItems = listBatchRun.write(List("Item 4", "Item 5"), saveItem)

  def main(args: Array[String]): Unit = {
    println(listCollection.wrap("Hello"))
    println(listCollection.first(List("Hello", "World", "!")))

    println(saveItem)
    println(saveItems)
  }
}
