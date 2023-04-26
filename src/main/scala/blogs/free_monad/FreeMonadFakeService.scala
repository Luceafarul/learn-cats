package blogs.free_monad

import cats.free.Free

// One more read article about free monad: https://underscore.io/blog/posts/2015/04/14/free-monads-are-simple.html
object FreeMonadFakeService {
  type UserId = Int
  type UserName = String
  type UserPhoto = String

  type Requestable[A] = Free[Request, A]

  final case class Tweet(userId: UserId, msg: String)
  final case class User(id: UserId, name: UserName, photo: UserPhoto)

  // Services represents web services we can call to fetch data
  sealed trait Service[A]
  final case class GetTweets(userId: UserId) extends Service[List[Tweet]]
  final case class GetUserName(userId: UserId) extends Service[UserName]
  final case class GetUserPhoto(userId: UserId) extends Service[UserPhoto]

  // A request represents a request for data
  sealed trait Request[A]

  object Request {
    final case class Pure[A](a: A) extends Request[A]
    final case class Fetch[A](service: Service[A]) extends Request[A]

    def pure[A](a: A): Requestable[A] =
      Free.liftF(Pure(a))

    def fetch[A](service: Service[A]): Requestable[A] =
      Free.liftF(Fetch(service))
  }

  import cats.{~>, Id}

  object ToyInterpreter extends (Request ~> Id) {
    import Request._

    def apply[A](fa: Request[A]): Id[A] =
      fa match {
        case Pure(a) => a
        case Fetch(service) =>
          service match {
            case GetTweets(userId) =>
              println(s"Getting tweets for user: $userId")
              List(Tweet(1, "Hi"), Tweet(2, "Hello, there!"), Tweet(3, "Good morning!"))
            case GetUserName(userId) =>
              println(s"Getting user name for user: $userId")
              userId match {
                case 1 => "Angie"
                case 2 => "Loern"
                case _ => "Nobody"
              }
            case GetUserPhoto(userId) =>
              println(s"Getting user photo for user: $userId")
              userId match {
                case 1 => ":-)"
                case 2 => ":-D"
                case _ => ":-|"
              }
          }
      }
  }

  object Example {
    import Request._

    val id: UserId = 1

    def getUser(id: UserId): Requestable[User] =
      for {
        name <- fetch(GetUserName(id))
        photo <- fetch(GetUserPhoto(id))
      } yield User(id, name, photo)

    import cats.syntax.traverse._

    val userTweets: Requestable[List[(String, User)]] =
      for {
        tweets <- fetch(GetTweets(id))
        result <- tweets.map { tweet =>
          for {
            user <- getUser(tweet.userId)
          } yield (tweet.msg -> user)
        }.sequence
      } yield result
  }

  def main(args: Array[String]): Unit =
    println(s"Result: ${Example.userTweets.foldMap(ToyInterpreter)}")
}
