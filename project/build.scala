import sbt._
import Keys._


object Settings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.scalamacros",
    version := "0.1.0",
    scalaVersion := "2.10.1",
    scalacOptions ++= Seq(),
    libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _)
  )
}

object build extends Build {
  import Dependencies._

  lazy val macros = Project(
    id = "ds-macros",
    base = file("macros"),
    settings = Settings.buildSettings
  ) dependsOn(main)

  lazy val main = Project(
    id = "ds-main",
    base = file("main"),
    settings = Settings.buildSettings ++ Seq(
      libraryDependencies += "com.google.appengine" % "appengine-api-1.0-sdk" % "1.8.0"
    )
  ) 
}

