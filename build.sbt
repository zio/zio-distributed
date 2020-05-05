import Dependencies._

ThisBuild / scalaVersion := "2.13.1"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "dev.zio"
ThisBuild / organizationName := "zio"
ThisBuild / licenses := List(
  "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
)
developers := List(
  Developer(
    "jdegoes",
    "John De Goes",
    "john@degoes.net",
    url("http://degoes.net")
  ),
  Developer(
    "francistoth",
    "Francis Toth",
    "tothfrancis@gmail.com",
    url("https://francistoth.github.io")
  ),
  Developer(
    "calvinlfer",
    "Calvin Lee Fernandes",
    "calvin.l.fer@gmail.com",
    url("https://blog.kaizen-solutions.io/")
  )
)

val zioVersion = "1.0.0-RC18-2"

lazy val root = (project in file("."))
  .settings(
    name := "zio-distributed",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"      % zioVersion,
      "dev.zio" %% "zio-test" % zioVersion % "test",
      "dev.zio"   %% "zio-test-sbt" % zioVersion % "test"
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
