package case_studies.test_async_code

import cats.Id
import cats.Applicative
import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.functor._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

trait UptimeClient[F[_]] {
  def uptime(hostname: String): F[Int]
}

trait RealUptimeClient extends UptimeClient[Future] {
  def uptime(hosstname: String): Future[Int]
}

class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
  def uptime(hostname: String): Int = hosts.getOrElse(hostname, 0)
}

class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
  def totalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.uptime).map(times => times.sum)
}
