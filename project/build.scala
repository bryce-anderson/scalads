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

lazy val project = Project (
    "project",
    file("."),
    settings = Settings.buildSettings
  ) aggregate(macros, appengine, mongodb)

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
      libraryDependencies += TypesafeConfig
    )
  ) dependsOn(macros % "test->test;compile->compile") 

  lazy val examples: Project = Project(
    id = "ds-examples",
    base = file("examples"),
    settings = Settings.buildSettings
  ) dependsOn(mongodb)
  
  lazy val appengineExample: Project = Project(
    id = "scalads-appeExample",
    base = file("appengine_example"),
    settings = Settings.buildSettings
  ) dependsOn(appengine)
}

