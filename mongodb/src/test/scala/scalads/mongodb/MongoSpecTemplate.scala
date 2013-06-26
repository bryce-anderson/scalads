package scalads.mongodb

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{BeforeAndAfterAll, FlatSpec}

import de.bwaldvogel.mongo.backend.memory.MemoryBackend
import de.bwaldvogel.mongo.MongoServer

import reactivemongo.api._

import concurrent.ExecutionContext.Implicits.global


/**
 * @author Bryce Anderson
 *         Created on 6/16/13
 */
trait MongoSpecTemplate extends FlatSpec with BeforeAndAfterAll with ShouldMatchers {

  private lazy val server: MongoServer = new MongoServer(new MemoryBackend())
  private lazy val serverAddress = server.bind()
  private lazy val driver = new MongoDriver
  private lazy val connection: MongoConnection = driver.connection(List("localubuntu:27017"))//driver.connection(List(serverAddress.getHostName + ":" + serverAddress.getPort))
  def db:DB = connection("testdb")

  override protected def afterAll() {
    connection.close()
    server.shutdownNow()
    super.afterAll()
  }
}


