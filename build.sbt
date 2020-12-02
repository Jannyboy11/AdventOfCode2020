val dottyVersion = "3.0.0-M1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "AdventOfCode2020",
    version := "0.1",

    scalaVersion := dottyVersion,
  )
