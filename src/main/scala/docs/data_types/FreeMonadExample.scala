package docs.data_types

import cats.InjectK
import docs.data_types.FreeMonadExample.Interact.Ask
import docs.data_types.FreeMonadExample.Interact.Tell
import docs.data_types.FreeMonadExample.DataOp.AddCat
import docs.data_types.FreeMonadExample.DataOp.GetAllCats
import docs.data_types.FreeMonadExample.Teletype.WriteLine
import docs.data_types.FreeMonadExample.Teletype.ReadLine
import cats.data.StateT
import cats.data.OptionT

object FreeMonadExample {
  // Let's imagine that we want to create a DSL for a key-value store.
  // We want to be able to do three things with keys:
  // - put a value into the store, associated with its key.
  // - get a value from the store given its key.
  // - delete a value from the store given its key.

  sealed trait KVStoreA[A]
  final case class Put[T](key: String, value: T) extends KVStoreA[Unit]
  final case class Get[T](key: String) extends KVStoreA[Option[T]]
  final case class Delete(key: String) extends KVStoreA[Unit]

  // There are five basic steps to "freeing" the ADT:
  // 1. Create a type based on Free[_] and KVStoreA[_].
  // 2. Create smart constructors for KVStore[_] using liftF.
  // 3. Build a program out of key-value DSL operations.
  // 4. Build a compiler for programs of DSL operations.
  // 5. Execute our compiled program.

  // 1. Create a Free type based on your ADT
  import cats.free.Free

  type KVStore[A] = Free[KVStoreA, A]

  // 2. Create smart constructors using liftF
  import cats.free.Free.liftF

  // Put and returns nothing (Unit)
  def put[T](key: String, value: T): KVStore[Unit] =
    liftF[KVStoreA, Unit](Put[T](key, value))

  // Get returns a T value
  def get[T](key: String): KVStore[Option[T]] =
    liftF[KVStoreA, Option[T]](Get[T](key))

  // Delete and returns nothing (Unit)
  def delete(key: String): KVStore[Unit] =
    liftF[KVStoreA, Unit](Delete(key))

  // Update compose get and put, and returns nothing
  def update[T](key: String, f: T => T): KVStore[Unit] =
    for {
      v <- get[T](key)
      _ <- v.map(elem => put[T](key, f(elem))).getOrElse(Free.pure())
    } yield ()

  // 3. Build a program
  def program: KVStore[Option[Int]] =
    for {
      _ <- put("cats", 2)
      _ <- update[Int]("cats", (_ + 5))
      _ <- put("dogs", 5)
      n <- get[Int]("cats")
      _ <- delete("cats")
    } yield n

  // 4. Write a compiler for your program
  import cats.arrow.FunctionK
  import cats.{~>, Id}
  import scala.collection.mutable

  // The program will crash if a type is incorrectly specified.
  def impureCompiler: KVStoreA ~> Id =
    new (KVStoreA ~> Id) { // The same as new FunctionK[KVStoreA, Id]
      // A very simple (and imprecise) key-value store
      val kvs = mutable.Map.empty[String, Any]

      def apply[A](fa: KVStoreA[A]): Id[A] =
        fa match {
          case Put(key, value) =>
            println(s"put($key, $value)")
            kvs(key) = value
            ()
          case Get(key) =>
            println(s"get($key)")
            kvs.get(key)
          case Delete(key) =>
            println(s"delete($key)")
            kvs.remove(key)
            ()
        }
    }

  // 5. Run your program
  val result: Option[Int] = program.foldMap(impureCompiler)

  // 6. Use a pure compiler (optional) and run also
  import cats.data.State
  type KVStoreState[A] = State[Map[String, Any], A]

  val pureCompiler: KVStoreA ~> KVStoreState = new (KVStoreA ~> KVStoreState) {

    def apply[A](fa: KVStoreA[A]): KVStoreState[A] =
      fa match {
        case Put(key, value) => State.modify(_.updated(key, value))
        case Get(key)        => State.inspect(_.get(key).asInstanceOf[A])
        case Delete(key)     => State.modify(_ - key)
      }
  }

  val pureResult = program.foldMap(pureCompiler).run(Map.empty).value

  // Composing Free monads ADTs.
  import cats.data.EitherK
  import cats.free.Free

  // Handles user interaction
  sealed trait Interact[A] extends Serializable with Product

  object Interact {
    final case class Ask(prompt: String) extends Interact[String]
    final case class Tell(msg: String) extends Interact[Unit]
  }

  // Represents persistence operations
  sealed trait DataOp[A] extends Serializable with Product

  object DataOp {
    final case class AddCat(c: String) extends DataOp[Unit]
    case object GetAllCats extends DataOp[List[String]]
  }

  type CatsApp[A] = EitherK[DataOp, Interact, A]

  class Interacts[F[_]](implicit I: InjectK[Interact, F]) {
    import Interact._

    def tell(msg: String): Free[F, Unit] = Free.liftInject[F](Tell(msg))
    def ask(prompt: String): Free[F, String] = Free.liftInject[F](Ask(prompt))
  }

  object Interacts {
    implicit def interacts[F[_]](implicit I: InjectK[Interact, F]): Interacts[F] = new Interacts[F]
  }

  class DataSource[F[_]](implicit I: InjectK[DataOp, F]) {
    import DataOp._

    def addCat(c: String): Free[F, Unit] = Free.liftInject[F](AddCat(c))
    def getAllCats: Free[F, List[String]] = Free.liftInject[F](GetAllCats)
  }

  object DataSource {
    implicit def dataSources[F[_]](implicit I: InjectK[DataOp, F]): DataSource[F] = new DataSource[F]
  }

  def catProgram(implicit I: Interacts[CatsApp], D: DataSource[CatsApp]): Free[CatsApp, Unit] = {
    import I._, D._

    for {
      cat <- ask("What's the kitty's name?")
      _ <- addCat(cat)
      cats <- getAllCats
      _ <- tell(cats.toString)
    } yield ()
  }

  // Define interperatators
  object ConsoleCatsInterpreter extends (Interact ~> Id) {

    def apply[A](fa: Interact[A]): Id[A] =
      fa match {
        case Ask(prompt) =>
          println(prompt)
          scala.io.StdIn.readLine()
        case Tell(msg) =>
          println(msg)
      }
  }

  object InMemoryDataSourceInterpreter extends (DataOp ~> Id) {
    private[this] val memoryDataSource = new mutable.ListBuffer[String]

    def apply[A](fa: DataOp[A]): Id[A] =
      fa match {
        case AddCat(c)  => memoryDataSource.append(c); ()
        case GetAllCats => memoryDataSource.toList
      }
  }

  val interpreter: CatsApp ~> Id = InMemoryDataSourceInterpreter.or(ConsoleCatsInterpreter)

  // FreeT
  import cats.free.FreeT

  // A base ADT for the user interaction without state semantics
  sealed trait Teletype[A] extends Serializable with Product

  object Teletype {
    final case class WriteLine(line: String) extends Teletype[Unit]
    final case class ReadLine(prompt: String) extends Teletype[String]
  }

  type TeletypeT[M[_], A] = FreeT[Teletype, M, A]
  type Log = List[String]
  type TeletypeState[A] = State[List[String], A]

  // Teletype smart constructors
  object TeletypeOps {
    import Teletype._

    def writeLine(line: String): TeletypeT[TeletypeState, Unit] =
      FreeT.liftF[Teletype, TeletypeState, Unit](WriteLine(line))

    def readLine(prompt: String): TeletypeT[TeletypeState, String] =
      FreeT.liftF[Teletype, TeletypeState, String](ReadLine(prompt))

    def log(msg: String): TeletypeT[TeletypeState, Unit] =
      FreeT.liftT[Teletype, TeletypeState, Unit](State.modify(msg :: _))
  }

  def teletypeProgram: TeletypeT[TeletypeState, Unit] =
    for {
      userSaid <- TeletypeOps.readLine("What's up?!")
      _ <- TeletypeOps.log(s"User said: $userSaid")
      _ <- TeletypeOps.writeLine("Thanks, see you soon!")
    } yield ()

  def teletypeInterpreter = new (Teletype ~> TeletypeState) {

    def apply[A](fa: Teletype[A]): TeletypeState[A] =
      fa match {
        case ReadLine(prompt) =>
          println(prompt)
          val userInput = "Hanging is here" // scala.io.StdIn.readLine()
          StateT.pure(userInput)
        case WriteLine(line) =>
          StateT.pure(println(line))
      }
  }

  // One more example of FreeT
  import cats.syntax.all._
  import scala.util.Try

  sealed trait Ctx[A] extends Serializable with Product

  object Ctx {
    final case class Action(value: Int) extends Ctx[Int]
  }

  import Ctx._

  def op1: FreeT[Ctx, Option, Int] = FreeT.liftF(Action(7))
  def op2: FreeT[Ctx, Option, Int] = FreeT.liftT(Option(3))
  def op3: FreeT[Ctx, Option, Int] = FreeT.pure(3)

  val onComplete: FreeT[Ctx, Option, Int] =
    for {
      a <- op1
      b <- op2
      c <- op3
    } yield a + b + c

  type OptTry[A] = OptionT[Try, A]

  def tryInterpreter: Ctx ~> OptTry = new (Ctx ~> OptTry) {

    def apply[A](fa: Ctx[A]): OptTry[A] =
      fa match {
        case Action(value) => OptionT.liftF(Try(value))
      }
  }

  def optTryLift: Option ~> OptTry = new (Option ~> OptTry) {

    def apply[A](fa: Option[A]): OptTry[A] =
      fa match {
        case Some(value) => OptionT(Try(Option(value)))
        case None        => OptionT.none
      }
  }

  def main(args: Array[String]): Unit = {
    println(s"Result of our program: $result")
    println(s"Result of our program: $pureResult")

    catProgram.foldMap(interpreter)

    // FreeT examples:
    import TeletypeOps._

    // First
    val state = teletypeProgram.foldMap(teletypeInterpreter)
    val initialState = Nil
    val (stored, _) = state.run(initialState).value
    println(stored)

    // Second
    val hoisted = onComplete.hoist(optTryLift)
    val evaluated = hoisted.foldMap(tryInterpreter)
    val res = evaluated.value
    println(res)
  }
}
