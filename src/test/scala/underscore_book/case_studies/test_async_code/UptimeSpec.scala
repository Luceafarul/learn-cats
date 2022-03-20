package underscore_book.case_studies.test_async_code

import book.case_studies.test_async_code.{TestUptimeClient, UptimeService}
import org.scalatest.{Matchers, WordSpec}

class UptimeSpec extends WordSpec with Matchers {
  "Uptime" should {
    "calculate total uptime" in {
        val hosts = Map("host1" -> 10, "host2" -> 7, "host3" -> 4)
        val client = new TestUptimeClient(hosts)
        val service = new UptimeService(client)
        val actual = service.totalUptime(hosts.keys.toList)
        val expected = hosts.values.sum
        actual shouldBe expected
    }
  }
}
