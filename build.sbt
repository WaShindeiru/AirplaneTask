ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

lazy val root = (project in file("."))
  .settings(
    name := "AirplaneTask"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % "test"
