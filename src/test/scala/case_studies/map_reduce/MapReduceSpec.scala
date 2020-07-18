package case_studies.map_reduce

import cats.instances.string._
import cats.instances.int._
import org.scalatest.{WordSpec, Matchers}

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
  }
}
