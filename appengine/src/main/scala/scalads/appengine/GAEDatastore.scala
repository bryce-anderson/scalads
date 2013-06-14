package scalads.appengine

import scala.reflect.macros.Context
import scala.language.experimental.macros

import scala.collection.JavaConverters._

import scalads.AbstractDatastore
import scalads.writers.Writer
import scalads.appengine.readers.GAEObjectReader
import scalads.appengine.writers.GAEWriter

import com.google.appengine.api.datastore.{Entity, Key,
                      DatastoreServiceFactory, DatastoreService, Query => GQuery}
import scala.reflect.ClassTag
import scalads.macroimpls.EntityBuilder

/**
 * @author Bryce Anderson
 *         Created on 6/9/13
 */
class GAEDatastore(val svc: DatastoreService) extends AbstractDatastore[Key, Entity] { self =>

  type QueryType[U] = GAEQuery[U]

  def withTransaction[U](f: => U): U = {
    val txn = svc.beginTransaction()
    try { val a = f; txn.commit(); a }
    finally { if(txn.isActive) txn.rollback() }
  }

  def delete(entity: Entity) {svc.delete(entity.getKey)}

  def newReader(entity: Entity): GAEObjectReader = new GAEObjectReader(entity, "")

  def newWriter(entity: Entity) = new GAEWriter(entity)

  override def put(entities: Iterable[Entity]) { svc.put(entities.asJava) }

  def putEntity(entity: Entity): Key = svc.put(entity)

  def putRaw(tpe: ClassTag[_], parent: Key)(f: (Writer[Any]) => Unit): Key = {
    val writer = new GAEWriter(new Entity(tpe.runtimeClass.getName, parent))
    f(writer)
    putEntity(writer.result)
  }

  def query[U](implicit clazz: ClassTag[U]): QueryType[U] = {
    new GAEQuery[U](self, new GQuery(clazz.runtimeClass.getName))
  }

}

object GAEDatastore {
  def getDatastoreService() = new GAEDatastore(DatastoreServiceFactory.getDatastoreService)

}
