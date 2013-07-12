
import sbt._


object Dependencies {
  private val appengineVersion = "1.8.0"

  lazy val Specs2 = "org.specs2" %% "specs2" % "1.13"
  lazy val ScalaTest = "org.scalatest" % "scalatest_2.10" % "1.9.1"
  lazy val GoogleAppEngine = "com.google.appengine" % "appengine-api-1.0-sdk" % appengineVersion
  lazy val GoogleAppEngineTest = "com.google.appengine" % "appengine-testing" % appengineVersion
  lazy val GoogleAppEngineStubs = "com.google.appengine" % "appengine-api-stubs" % appengineVersion
  lazy val GoogleAppEngineLabs = "com.google.appengine" % "appengine-api-labs" % appengineVersion
  
  lazy val TypesafeConfig = "com.typesafe" % "config" % "1.0.1"
  
  lazy val MongoDBDriver = "org.reactivemongo" %% "reactivemongo" % "0.9"
  // lazy val MongoMemoryDB = "de.bwaldvogel" % "mongo-java-server" % "1.1.1"
  lazy val PlayIteratees = "play" %% "play-iteratees" % "2.1.0"
  
  // Dependencies for the examples
  lazy val scalatra_2_2_1 = "org.scalatra" % "scalatra_2.10" % "2.2.1"
  lazy val scalatra_2_0_5 = "org.scalatra" % "scalatra_2.10" % "2.0.5"
  lazy val jetty = "org.eclipse.jetty" % "jetty-webapp" % "8.1.10.v20130312" 
  lazy val servlet = "javax.servlet" % "javax.servlet-api" % "3.0.1"
  
}
