import sbt._
import Keys._

import Dependencies._

object Settings {
  
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.scalamacros",
    version := "0.1.0",
    scalaVersion := "2.10.1",
    scalacOptions ++= Seq(),
    libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _),
    libraryDependencies += ScalaTest % "test"
  )
}

object build extends Build {

  lazy val macros: Project = Project(
    id = "ds-macros",
    base = file("macros"),
    settings = Settings.buildSettings ++ Seq(parallelExecution in Test := false) ++ Seq(
      libraryDependencies += GoogleAppEngine
    )
  )
  
  lazy val appengine: Project = Project(
    id = "ds-appengine",
    base = file("appengine"),
    settings = Settings.buildSettings ++ Seq(parallelExecution in Test := false) ++ Seq(
      libraryDependencies += GoogleAppEngine,
      libraryDependencies += GoogleAppEngineTest % "test",
      libraryDependencies += GoogleAppEngineStubs % "test",
      libraryDependencies += GoogleAppEngineLabs % "test"
    )
  ) dependsOn(macros)
  
}

