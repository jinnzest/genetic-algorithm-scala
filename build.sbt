
name := "genetic-algorithm"

version := "0.1-SNAPSHOT"

val scala3Version = "3.7.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Scala 3 Project Template",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalatestplus" %% "scalacheck-1-17" % "3.2.18.0" % Test,
      "org.scalatest" %% "scalatest" % "3.2.18" % Test,
      "org.scalamock" %% "scalamock" % "7.5.5" % Test
    )
  )

assembly/assemblyJarName := "genetic-algorithm"