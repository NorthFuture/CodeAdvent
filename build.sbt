lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.7",
      version := "0.1.0-SNAPSHOT"
    )),
    name := "Advent of Code",

  )

/*
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.6"

enablePlugins(ScalaJSPlugin)

scalaJSUseMainModuleInitializer := true

mainClass in Compile := Some("Year2018.Day10")
*/