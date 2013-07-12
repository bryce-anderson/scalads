import Dependencies._

name := "scalads-examples"

description := "Examples of using ScalaDS with Scalatra"

// Add the webplugin

seq(sbtappengine.Plugin.webSettings :_*)

TaskKey[Unit]("gc") := {
  println("requesting garbage collection")
  System.gc()
}

libraryDependencies ++= Seq(
  scalatra_2_2_1,
  jetty % "container",
  MongoDBDriver,
  servlet
)