package appengine

import appengine.GAESpecTemplate
import scalads.macroimpls
import com.google.appengine.api.datastore.FetchOptions.Builder.withLimit
import com.google.appengine.api.datastore.{Entity, Query, DatastoreServiceFactory}

import scalads.AbstractDatastore
import scalads.appengine.readers.GAEObjectReader
import scalads.appengine.writers.GAEWriter
import org.scalatest.{BeforeAndAfter, FlatSpec}
import org.scalatest.matchers.ShouldMatchers
import scalads.appengine.GAEDatastore


/**
 * @author Bryce Anderson
 *         Created on 5/27/13
 */

class SerializerSpec extends FlatSpec with BeforeAndAfter with ShouldMatchers {

  case class Simple(in: Int, in2: String)

  "DSWriter" should  "Write a simple entity" in {

    val a = Simple(0, "one")
    val writer = new GAEWriter(new Entity("appengine.SerializerSpec.Simple"))
    macroimpls.Serializer.serialize(a, writer)
    val ds = DatastoreServiceFactory.getDatastoreService()
    ds.put(writer.result)
    ds.prepare(new Query("appengine.SerializerSpec.Simple")).countEntities(withLimit(10)) should equal (1)
  }

  it should "Store longer strings" in {
    case class SuperString(str: String)

    val a = SuperString("hello"*500)
    val ds = GAEDatastore.getDatastoreService()

    ds.put(a)

    ds.query[SuperString]
      .getIterator.next() should equal(a)
  }

  it should "Store a compound object" in {
    case class Compound(name: String, simple: Simple, end: String)

    val a = Compound("Compound", Simple(0, "one"), "the end")

    val writer = new GAEWriter(new Entity("appengine.SerializerSpec.Compound"))
    macroimpls.Serializer.serialize(a, writer)
    val entity = writer.result

    println(entity)
    println(Simple.getClass)

    val ds = DatastoreServiceFactory.getDatastoreService()
    val txn = ds.beginTransaction()
    try {
      ds.put(entity)
      txn.commit()
    } finally {
      if (txn.isActive) txn.rollback()
    }

    ds.prepare(new Query("appengine.SerializerSpec.Compound")).countEntities(withLimit(10)) should equal (1)
  }

  "Serializer" should "serialize objects with Lists" in {
    case class WithList(name: String, lst: List[Simple])

    val ds = DatastoreServiceFactory.getDatastoreService()

    val a = WithList("Bill", Simple(1, "one")::Simple(2, "two")::Nil)
    val writer = new GAEWriter(new Entity("appengine.SerializerSpec.Compound"))
    macroimpls.Serializer.serialize(a, writer)

    val key = ds.put(writer.result)

    val entity = ds.get(key)
    val reader = new GAEObjectReader(entity, "")
    val result = macroimpls.Deserializer.deserialize[WithList](reader)

    result should equal(a)
  }
}
