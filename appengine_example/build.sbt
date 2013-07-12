import Dependencies._
import sbtappengine.{Plugin=>gae}

name := "scalads-appeExample"

description := "Example of using ScalaDS with Scalatra"

// Add the webplugin

seq(sbtappengine.Plugin.webSettings :_*)

TaskKey[Unit]("gc") := {
  println("requesting garbage collection")
  System.gc()
}

libraryDependencies ++= Seq(
  scalatra_2_0_5,
  "javax.servlet" % "servlet-api" % "2.5" % "provided",
  "org.eclipse.jetty" % "jetty-webapp" % "7.6.8.v20121106" % "container"
)