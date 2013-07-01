import Dependencies._

name := "scalads-examples"

description := "Examples of using ScalaDS with Scalatra"

// Add the webplugin

seq(webSettings :_*)

TaskKey[Unit]("gc") := {
  println("requesting garbage collection")
  System.gc()
}

libraryDependencies ++= Seq(
  scalatra_2_10,
  jetty % "container",
  MongoDBDriver,
  servlet
)