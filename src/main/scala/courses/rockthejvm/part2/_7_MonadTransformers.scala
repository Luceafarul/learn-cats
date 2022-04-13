package courses.rockthejvm.part2

import scala.concurrent.Future

object _7_MonadTransformers extends App {
  def sumAllOptions(xs: List[Option[Int]]): Int =
    xs.collect { case Some(x) => x }.sum

  println(sumAllOptions(List(Some(1), Some(2), None, Some(4), Some(5))))


  // Option transformer

  import cats.data.OptionT

  def sumAllOptionsT(xs: List[Option[Int]]): Int = {
    import cats.implicits._
    OptionT(xs).sumAll
  }

  println(sumAllOptionsT(List(Some(1), Some(2), None, Some(4), Some(5))))

  val listOfNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))
  val listOfTuples: OptionT[List, (Int, Char)] =
    for {
      n <- listOfNumberOptions
      c <- listOfCharOptions
    } yield (n, c)

  println(listOfTuples)
  println(listOfTuples.value)

  // Either transformer

  import cats.data.EitherT
  import scala.concurrent.ExecutionContext.Implicits.global

  val listOfEithers: EitherT[List, String, Int] = EitherT(List(Left("Something went wrong..."), Right(13), Right(37)))
  val futureOfEither: EitherT[Future, String, Int] = EitherT[Future, String, Int](Future(Right(73)))

  println(listOfEithers)
  println(futureOfEither)

  println(EitherT.right[String](Option.empty[Int]))
  println(EitherT.right[String](Option(10)))

  // Exercise:
  // We have a multi-machine cluster for your business which will receive a traffic surge
  //  following a media appearance.
  // We measure bandwidth in units.
  // We want to allocate TWO of our servers to cope with the traffic spike.
  // We know the current capacity for each server and we know we'll hold the traffic
  //  if the sum of bandwidths is > 250
  val bandwidths = Map(
    "server1.media.com" -> 75,
    "server2.media.com" -> 330,
    "server3.media.com" -> 100,
  )
  val MaxBandwidth = 250

  type AsyncResponse[T] = EitherT[Future, String, T] // Wrapper over Future[Either[String, T]]

  def bandwidth(server: String): AsyncResponse[Int] =
    bandwidths.get(server) match {
      case Some(s) => EitherT.right(Future(s))
      case None => EitherT.left(Future(s"Server: $server is unreachable"))
    }

  def canWithStandSurge(s1: String, s2: String): AsyncResponse[Boolean] =
    for {
      band1 <- bandwidth(s1)
      band2 <- bandwidth(s2)
    } yield (band1 + band2) > MaxBandwidth

  def generateTrafficSpike(s1: String, s2: String): AsyncResponse[String] =
    canWithStandSurge(s1, s2).transform {
      case Right(false) => Left("Fail: bandwidths is lower than 250")
      case Right(true) => Right("Success. We can generate traffic.")
      case Left(value) => Left(s"Fail: $value")
    }

  generateTrafficSpike("server1.media.com", "server3.media.com").value.foreach(println)
  generateTrafficSpike("server1.media.com", "server2.media.com").value.foreach(println)
  generateTrafficSpike("server1.media.com", "server4.media.com").value.foreach(println)
}
