package scalads.appengine

import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.reflect.runtime.universe.{TypeTag, typeTag}

import scala.collection.JavaConverters._

import scalads.AbstractDatastore
import scalads.readers.ObjectReader
import scalads.writers.Writer
import scalads.core.EntityBacker
import scalads.appengine.readers.GAEObjectReader
import scalads.appengine.writers.GAEWriter

import com.google.appengine.api.datastore.{Entity => GEntity, Key => GKey,
                      DatastoreServiceFactory, DatastoreService, Query => GQuery}

/**
 * @author Bryce Anderson
 *         Created on 6/9/13
 */
class GAEDatastore(val svc: DatastoreService) extends AbstractDatastore[GKey, GEntity] { self =>
  type Entity = GEntity
  type Key = GKey
  type Repr = GAEDatastore

  def withTransaction[U](f: => U): U = {
    val txn = svc.beginTransaction()
    try { val a = f; txn.commit(); a }
    finally { if(txn.isActive) txn.rollback() }
  }

  def delete(entity: Entity) {svc.delete(entity.getKey)}

  def newReader(entity: Entity): GAEObjectReader = new GAEObjectReader(entity, "")

  def newWriter(entity: Entity) = new GAEWriter(entity)

  def put(entities: Iterable[Entity]) { svc.put(entities.asJava) }

  def putWithParent[U](obj: U, key: Key) = macro GAEDatastore.putWithParent[U]

  def putEntity(entity: Entity): Key = svc.put(entity)

  def put(parent: Key, f: (Writer[Any]) => Unit): Key = {
    val writer = new GAEWriter(new Entity(null, parent))
    f(writer)
    putEntity(writer.result)
  }

  def mapQuery[U](tpe: String)(f: (AbstractDatastore[_, Entity], ObjectReader) => U with EntityBacker[U, Entity]): GAEQuery[U] = {

    new GAEQuery[U](self, new GQuery(tpe), f)
  }

  def query[U]: GAEQuery[U] = macro AbstractDatastore.queryImpl[U, Entity, GAEQuery[U]]
}

object GAEDatastore {
  def getDatastoreService() = new GAEDatastore(DatastoreServiceFactory.getDatastoreService)

  def putWithParent[U: c.WeakTypeTag](c: Context { type PrefixType = GAEDatastore})(obj: c.Expr[U], key: c.Expr[GKey]): c.Expr[Unit] = {
    import c.universe._
    import scalads.macroimpls.Serializer.serializeImpl


    val deserializer = serializeImpl(c)(obj, c.Expr[Writer[GEntity]](Ident(newTermName("writer"))))
    reify {
      c.prefix.splice.put(key.splice, writer => deserializer.splice)
    }
  }
}
