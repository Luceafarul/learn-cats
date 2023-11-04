package docs.data_types

import cats.implicits._
// import cats.effect.implicits._
import cats.data.{EitherT, RWST}
import cats.data.IndexedReaderWriterStateT
import cats.effect.IO
import cats.data.ReaderWriterStateT
import cats.kernel.Monoid

object IndexedRWSTExample extends App {
  // IndexedReaderWriterStateT[F[_], E, L, SA, SB, A]
  // Represents a stateful computation in a context F[_],
  // from state SA to state SB, with an initial environment E,
  // an accumulated log L and a result A.
  // In other words, it is a pre-baked stack of ReaderT[F, E, A], WriterT[F, L, A] and IndexedStateT[F, SA, SB, A].
  //
  // IndexedReaderWriterStateT[
  //    F[_] - context,
  //    E - initial environment,
  //    L - acculated log,
  //    SA - from state,
  //    SB - to state,
  //    A - result
  // ]
  // type RWST[F[_], E, L, S, A] = ReaderWriterStateT[F, E, L, S, A]
  // type ReaderWriterStateT[F[_], E, L, S, A] = IndexedReaderWriterStateT[F, E, L, S, S, A]
  // Represents a stateful computation in a context F[_],
  // over state S, with an initial environment E,
  // an accumulated log L and a result A.

  // Ohay, let's try basic
  final case class SimpleState(name: String, numbers: List[Int]) extends Product with Serializable

  object SimpleState {
    def empty: SimpleState = SimpleState("", List.empty[Int])

    implicit val monoidInstance: Monoid[SimpleState] = new Monoid[SimpleState] {
      def empty: SimpleState = ???
      def combine(x: SimpleState, y: SimpleState): SimpleState = ???
    }
  }

  type SimpleIRWST[T] = RWST[IO, String, List[String], Option[SimpleState], T]

  // Example from IndexedReaderWriterStateT code docs:
  type ErrorOr[A] = Either[String, A]
  type Env = String
  type Log = List[String]
  val xError: IndexedReaderWriterStateT[ErrorOr, Env, Log, Int, Int, Int] = IndexedReaderWriterStateT.get
  val xOpt: IndexedReaderWriterStateT[Option, Env, Log, Int, Int, Int] = IndexedReaderWriterStateT.get

  xError.transformF(_.toOption)
  val input = 7
  val res01 = xError.run("TestEnv", input)
  println(res01)
  val res02 = xOpt.run("TestEnv", input)
  println(res02)

  val modifiedState = xError.modify(n => n + 4)
  val res03 = modifiedState.run("TestEnv", input)
  println(res03)

  type State = Option[SimpleState]

  // val simpleRwst: SimpleIRWST[State] =
  val simpleRwst: IndexedReaderWriterStateT[IO, Env, Log, State, State, State] =
    IndexedReaderWriterStateT.get

  val modifiedSimpleRwst = simpleRwst.modify { state =>
    state.map(simpleState => SimpleState("Updated name", simpleState.numbers :+ 1))
  }

  val secondModifiedSimpleRwst = modifiedSimpleRwst.modify { state =>
    state.map { simpleState => SimpleState(simpleState.name, simpleState.numbers :+ 2) }
  }

  //(
  //  List(),
  //  Some(SimpleState(Updated name,List(1, 2))),
  //  Some(SimpleState(,List()))
  //)
  println(secondModifiedSimpleRwst.runS("TestEnv", Option(SimpleState.empty)).unsafeRunSync())
}
