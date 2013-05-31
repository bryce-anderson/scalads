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
    libraryDependencies += ScalaTest % "test",
    libraryDependencies += GoogleAppEngineTest % "test",
    libraryDependencies += GoogleAppEngineStubs % "test",
    libraryDependencies += GoogleAppEngineLabs % "test"
  )
}

object build extends Build {
  

  lazy val macros: Project = Project(
    id = "ds-macros",
    base = file("macros"),
    settings = Settings.buildSettings
  ) dependsOn(main)

  lazy val main: Project = Project(
    id = "ds-main",
    base = file("main"),
    settings = Settings.buildSettings ++ Seq(
      libraryDependencies += GoogleAppEngine
    ) 
  )
}

