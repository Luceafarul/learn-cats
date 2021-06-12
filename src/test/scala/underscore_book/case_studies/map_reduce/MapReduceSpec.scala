package underscore_book.case_studies.map_reduce

import cats.instances.string._
import cats.instances.int._
import org.scalatest.{WordSpec, Matchers}
import scala.concurrent._
import scala.concurrent.duration._

class MapReduceSpec extends WordSpec with Matchers {
  "MapReduce" should {
    "apply function f and reduce with foldMap" in {
      val result01 = MapReduce.foldMap(Vector(1, 2, 3, 4, 5))(identity)
      val result02 = MapReduce.foldMap(Vector(1, 2, 3, 4, 5))(_.toString + "! ")
      val result03 = MapReduce.foldMap("Hello, Scala!".toVector)(_.toString.toUpperCase)

      result01 shouldBe 15
      result02 shouldBe "1! 2! 3! 4! 5! "
      result03 shouldBe "HELLO, SCALA!"
    }

    "apply function f and reduce with foldMapCats" in {
      val result01 = MapReduce.foldMapCats(Vector(1, 2, 3, 4, 5))(identity)
      val result02 = MapReduce.foldMapCats(Vector(1, 2, 3, 4, 5))(_.toString + "! ")
      val result03 = MapReduce.foldMapCats("Hello, Scala!".toVector)(_.toString.toUpperCase)

      result01 shouldBe 15
      result02 shouldBe "1! 2! 3! 4! 5! "
      result03 shouldBe "HELLO, SCALA!"
    }

    "apply f function and reduce in parallel with parallelFoldMap or parallelFoldMapCats" in {
      val result01: Future[Int] = MapReduce.parallelFoldMap((1 to 1000000).toVector)(identity)
      val result02: Future[Int] = MapReduce.parallelFoldMapCats((1 to 1000000).toVector)(identity)

      // TODO use await spec for it
      Await.result(result01, 1.seconds) shouldBe 1784293664
      Await.result(result02, 1.seconds) shouldBe 1784293664
    }
  }
}
