package macros

import org.scalatest._
import writers.GAEDSWriter
import com.google.appengine.tools.development.testing.{LocalDatastoreServiceTestConfig, LocalServiceTestHelper}
import com.google.appengine.api.datastore.FetchOptions.Builder.withLimit
import org.scalatest.matchers.ShouldMatchers
import com.google.appengine.api.datastore.{Query, DatastoreServiceFactory}


/**
 * @author Bryce Anderson
 *         Created on 5/27/13
 */

class SerializerSpec extends FlatSpec with BeforeAndAfter with ShouldMatchers {

  val helper = new LocalServiceTestHelper(new LocalDatastoreServiceTestConfig())

  after { helper.tearDown() }

  before {  helper.setUp() }

  case class Simple(in: Int, in2: String)

  "DSWriter" should  "Write a simple entity" in {

    val a = Simple(0, "one")
    val writer = new GAEDSWriter("Simple")
    macroimpls.Serializer.serialize(a, writer)
    val ds = DatastoreServiceFactory.getDatastoreService()
    ds.put(writer.result.head)

  }

  it should "Store a compound object" in {
    case class Compound(name: String, simple: Simple, end: String)

    val a = Compound("Compound", Simple(0, "one"), "the end")
    val writer = new GAEDSWriter("Compound")
    macroimpls.Serializer.serialize(a, writer)
    val entities = writer.result

    println(entities)
    println(Simple.getClass)

    val ds = DatastoreServiceFactory.getDatastoreService()
    val txn = ds.beginTransaction()
    try {
      entities.foreach(ds.put)
      txn.commit()
    } finally {
      if (txn.isActive) txn.rollback()
    }

    ds.prepare(new Query("Compound")).countEntities(withLimit(10)) should equal (1)
    ds.prepare(new Query(Simple.getClass.toString)).countEntities(withLimit(10)) should equal (1)
  }
}
