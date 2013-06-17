package scalads.mongodb

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{BeforeAndAfterAll, FlatSpec}

import com.mongodb.{ServerAddress, MongoClient}

import de.bwaldvogel.mongo.backend.memory.MemoryBackend
import de.bwaldvogel.mongo.MongoServer


/**
 * @author Bryce Anderson
 *         Created on 6/16/13
 */
trait MongoSpecTemplate extends FlatSpec with BeforeAndAfterAll with ShouldMatchers {

  private lazy val server: MongoServer = new MongoServer(new MemoryBackend())
  private lazy val serverAddress = server.bind()
  private lazy val client = new MongoClient(new ServerAddress(serverAddress))
  def coll = client.getDB("testdb").getCollection("testcollection")

  override protected def afterAll() {
    client.close()
    server.shutdownNow()
    super.afterAll()
  }

//
//  override protected def beforeAll() {
//    println(s"Current Thread: ${Thread.currentThread().getId}")
//    server = new MongoServer(new MemoryBackend())
//
//    // bind on a random local port
//    val serverAddress = server.bind()
//
//    client = new MongoClient(new ServerAddress(serverAddress))
//    coll = client.getDB("testdb").getCollection("testcollection")
//    println("Done connecting")
//    super.beforeAll()
//  }

}

//object MongoSpecTemplate {
//  private var count = 0
//  private val lock = new Object
//
//  def setUp(server: MongoServer, client: MongoClient, coll: DBCollection) = lock.synchronized {
//    count += 1
//    println(s"The count is: $count")
//    if(count == 1) {
//      val nserver = new MongoServer(new MemoryBackend())
//      // bind on a random local port
//      val serverAddress = nserver.bind()
//      val nclient = new MongoClient(new ServerAddress(serverAddress))
//
//      (nserver, nclient, nclient.getDB("testdb").getCollection("testcollection"))
//    } else (server, client, coll)
//
//  }
//
//  def tearDown(client: MongoClient, server: MongoServer) = lock.synchronized {
//    count -= 1
//    if (count == 0) {
//      client.close()
//      server.shutdownNow()
//    }
//  }
//}

