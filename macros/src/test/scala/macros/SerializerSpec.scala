package macros

import writers.GAEDSWriter
import com.google.appengine.api.datastore.FetchOptions.Builder.withLimit
import com.google.appengine.api.datastore.{Entity, Key, Query, DatastoreServiceFactory}

import util.Datastore


/**
 * @author Bryce Anderson
 *         Created on 5/27/13
 */

class SerializerSpec extends GAESpecTemplate {

  case class Simple(in: Int, in2: String)

  "DSWriter" should  "Write a simple entity" in {

    val a = Simple(0, "one")
    val writer = new GAEDSWriter(new Entity("macros.SerializerSpec.Simple"))
    macroimpls.Serializer.serialize(a, writer)
    val ds = DatastoreServiceFactory.getDatastoreService()
    ds.put(writer.result)
    ds.prepare(new Query("macros.SerializerSpec.Simple")).countEntities(withLimit(10)) should equal (1)
  }

  it should "Store longer strings" in {
    case class SuperString(str: String)

    val a = SuperString("hello"*500)
    val ds = Datastore.getDatastoreService()

    ds.put(a)

    ds.query[SuperString]
      .getIterator.next() should equal(a)
  }

  it should "Store a compound object" in {
    case class Compound(name: String, simple: Simple, end: String)

    val a = Compound("Compound", Simple(0, "one"), "the end")

    val writer = new GAEDSWriter(new Entity("macros.SerializerSpec.Compound"))
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

    ds.prepare(new Query("macros.SerializerSpec.Compound")).countEntities(withLimit(10)) should equal (1)
  }
}
