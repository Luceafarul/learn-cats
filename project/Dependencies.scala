import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.11"
  lazy val catsCore = "org.typelevel" %% "cats-core" % "2.9.0"
  lazy val catsFree = "org.typelevel" %% "cats-free" % "2.9.0"
  lazy val catsEffect = "org.typelevel" %% "cats-effect" % "2.5.3"
}
