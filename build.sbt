ThisBuild / scalaVersion := "3.1.3"
ThisBuild / organization := "com.example"

lazy val hello = (project in file("."))
  .settings(
    name := "UCC-Scala-03",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % Test
  )