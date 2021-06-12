import Dependencies._

ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "cats-demo",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      catsCore,
      catsEffect
    )
  )
