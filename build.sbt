ThisBuild / scalaVersion := "2.13.10"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "adventofcode.2022"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "2022",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.0.3",
      "dev.zio" %% "zio-streams" % "2.0.3",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
      "dev.zio" %% "zio-test" % "2.0.3" % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
