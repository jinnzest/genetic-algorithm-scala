
name := "genetic-algorithm"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.3"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.13.4" % Test,
  "org.scalatest" %% "scalatest" % "3.0.1" % Test,
  "org.scalamock" %% "scalamock-scalatest-support" % "3.5.0" % Test
)

packAutoSettings