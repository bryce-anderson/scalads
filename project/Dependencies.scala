
import sbt._


object Dependencies {
  private val appengineVersion = "1.8.0"

  lazy val Specs2 = "org.specs2" %% "specs2" % "1.13"
  lazy val ScalaTest = "org.scalatest" % "scalatest_2.10" % "1.9.1"
  lazy val GoogleAppEngine = "com.google.appengine" % "appengine-api-1.0-sdk" % appengineVersion
  lazy val GoogleAppEngineTest = "com.google.appengine" % "appengine-testing" % appengineVersion
  lazy val GoogleAppEngineStubs = "com.google.appengine" % "appengine-api-stubs" % appengineVersion
  lazy val GoogleAppEngineLabs = "com.google.appengine" % "appengine-api-labs" % appengineVersion
  
  lazy val MongoDBDriver = "org.mongodb" % "mongo-java-driver" % "2.11.1"
  
}
