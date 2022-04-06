package courses.rockthejvm.part3

import scala.annotation.tailrec

object _2_Writers extends App {

  import cats.data.Writer

  // 1. Define them at the start
  val aWriter: Writer[List[String], Int] = Writer(List("Initial value is 37"), 37)

  // 2. Manipulation them with pure FP
  val anIncreasedWriter = aWriter.map(_ + 1) // Value increased, logs stay the same
  val aLogsWriter = anIncreasedWriter.mapWritten(_ :+ "Was increased by 1") // Value stays the same, logs changes
  val aWriterWithBothOne = aLogsWriter.bimap(_ :+ "Was increased by 1", _ + 1)
  val aWriterWithBothTwo = aWriterWithBothOne.mapBoth { (logs, value) =>
    (logs :+ "Was increased by 1", value + 1)
  }

  // 3. Dump either the value or the logs, or both
  val desiredValue = aWriterWithBothTwo.value
  val desiredLogs = aWriterWithBothTwo.written
  val (l, v) = aWriterWithBothTwo.run

  println(desiredValue)
  println(desiredLogs)
  println(aWriterWithBothTwo.run)

  // flatMap
  val writerA = Writer(Vector("Log A1", "Log A2"), 13)
  val writerB = Writer(Vector("Log B2"), 37)
  val compositeWriter =
    for {
      va <- writerA
      vb <- writerB
    } yield va + vb

  println(compositeWriter.run)

  // Reset the logs - clear the logs and keep the value inside
  val emptyWriter = compositeWriter.reset
  println(emptyWriter.run)

  // Exercise:
  // rewrite a function which "prints" things with writers
  def countAndSay(n: Int): Unit = {
    if (n <= 0) println("Starting!")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }

  countAndSay(10)

  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    @tailrec
    def loop(n: Int, writer: Writer[Vector[String], Int]): Writer[Vector[String], Int] =
      if (n <= 0) writer.mapWritten("Starting!" +: _)
      else loop(n - 1, writer.bimap(n.toString +: _, _ - 1))

    loop(n, Writer(Vector.empty, n))
  }

  def countAndLog2(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("Starting!"), n)
    else countAndLog2(n - 1).flatMap(_ => Writer(Vector(n.toString), n))
  }

  countAndLog(10).written.foreach(println)

  // Exercise:
  // Rewrite naiveSum with writers
  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum(${n - 1}) = $lowerSum")
      lowerSum + n
    }
  }

  naiveSum(5)

  def naiveSumWithLogs(n: Int): Writer[Vector[String], Int] =
    if (n <= 0) Writer(Vector.empty, n)
    else {
      val lowerSum = naiveSumWithLogs(n - 1)
      lowerSum.bimap(s"Now at $n" +: _ :+ s"Computed sum(${n - 1}) = ${lowerSum.value}", _ => lowerSum.value + n)
    }

  def solutionNaiveSumWithLogs(n: Int): Writer[Vector[String], Int] =
    if (n <= 0) Writer(Vector.empty, n)
    else for {
      _ <- Writer(Vector(s"Now at $n"), n)
      lowerSum <- solutionNaiveSumWithLogs(n - 1)
      _ <- Writer(Vector(s"Computed sum(${n - 1}) = $lowerSum"), n)
    } yield lowerSum + n

  def howForWorksNaiveSumWithLogs(n: Int): Writer[Vector[String], Int] =
    if (n <= 0) Writer(Vector.empty, n)
    else {
      Writer(Vector(s"Now at $n"), n).flatMap { _ =>
        solutionNaiveSumWithLogs(n - 1).flatMap { lowerSum =>
          Writer(Vector(s"Computed sum(${n - 1}) = $lowerSum"), n)
        }
      }
    }

  naiveSumWithLogs(5).written.foreach(println)
  solutionNaiveSumWithLogs(5).written.foreach(println)
  howForWorksNaiveSumWithLogs(5).written.foreach(println)

  // Benefit #1: We work with pure FP
  // Benefit #2: Writers can keep logs separate on multiple threads
}
