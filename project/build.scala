import sbt._
import Keys._

import Dependencies._

object Settings {
  
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.scalamacros",
    version := "0.1.0",
    scalaVersion := "2.10.2-RC1",
    scalacOptions ++= Seq(),
    libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _),
    libraryDependencies += "play" %% "play-iteratees" % "2.1.0",
    libraryDependencies += ScalaTest % "test",
    resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"
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
  ) dependsOn(macros % "test->test;compile->compile")
  
  lazy val mongodb: Project = Project(
    id = "ds-mongodb",
    base = file("mongodb"),
    settings = Settings.buildSettings ++ Seq(parallelExecution in Test := false) ++ Seq(
      libraryDependencies += MongoDBDriver,
      libraryDependencies += MongoMemoryDB % "test"
    )
  ) dependsOn(macros % "test->test;compile->compile")
  
}

