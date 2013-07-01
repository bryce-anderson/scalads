package myapp

import org.scalatra.ScalatraServlet
import scalads.mongodb.MongoDatastore
import reactivemongo.api.{DB, MongoConnection, MongoDriver}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/**
 * @author Bryce Anderson
 *         Created on 6/30/13
 */


case class Test(name: String, age: Int)


class Main extends ScalatraServlet {

  private lazy val ds = {
    import com.typesafe.config.ConfigFactory

    val config = ConfigFactory.load()
    val driver = new MongoDriver
    val connection: MongoConnection = driver.connection(List(config.getString("mongo.address")+":"+config.getInt("mongo.port")))
    def db:DB = connection("testdb")

    val username = config.getString("mongo.username")
    if (username != "null") {
      db.authenticate(username, config.getString("mongo.pass"))(3.seconds)
    }

    new MongoDatastore(db)
  }

  get("/hello") {
    val query = ds.query[Test]

    ds.put(new Test("Bryce", 28))


    "Hello world!" + query.getIterator.mkString(", ")

  }
}
