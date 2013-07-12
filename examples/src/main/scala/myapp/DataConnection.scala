package myapp

import reactivemongo.api.{DB, MongoConnection, MongoDriver}
import scalads.mongodb.MongoDatastore

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import java.util.concurrent.ForkJoinPool

/**
 * @author Bryce Anderson
 *         Created on 7/10/13
 */
trait DataConnection {
  def ds = DataConnection.ds
}

object DataConnection {

  // Give ourselves 20 threads on which to perform datastore ops
  private implicit val ec = ExecutionContext.fromExecutor(new ForkJoinPool(20))

  private lazy val ds = {
    import com.typesafe.config.ConfigFactory

    val config = ConfigFactory.load()
    val driver = new MongoDriver
    val connection: MongoConnection = driver.connection(List(config.getString("mongo.address")+":"+config.getInt("mongo.port")))
    def db:DB = connection(config.getString("mongo.dbName"))

    val username = config.getString("mongo.username")
    if (username != "null") {
      db.authenticate(username, config.getString("mongo.pass"))(3.seconds)
    }

    new MongoDatastore(db)
  }
}
