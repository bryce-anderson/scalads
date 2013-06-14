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
import scalads.macroimpls.EntityMaker

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

  def put(entities: Iterable[Entity]) { svc.put(entities.asJava) }

  def putWithParent[U](obj: U, key: Key)(implicit clazz: ClassTag[U]) = macro GAEDatastore.putWithParent[U]

  def putEntity(entity: Entity): Key = svc.put(entity)

  def putRaw(tpe: ClassTag[_], parent: Key)(f: (Writer[Any]) => Unit): Key = {
    val writer = new GAEWriter(new Entity(tpe.runtimeClass.getName, parent))
    f(writer)
    putEntity(writer.result)
  }

  def query[U](implicit clazz: ClassTag[U], entityMaker: EntityMaker[U, Entity]): QueryType[U] = {
    new GAEQuery[U](self, new GQuery(clazz.runtimeClass.getName), entityMaker)
  }

//
//  def mapQuery[U](clazz: ClassTag[U])(f: (AbstractDatastore[_, Entity], ObjectReader) => U with EntityBacker[U, Entity]): GAEQuery[U] = {
//    new GAEQuery[U](self, new GQuery(clazz.runtimeClass.getName), f)
//  }

}

object GAEDatastore {
  def getDatastoreService() = new GAEDatastore(DatastoreServiceFactory.getDatastoreService)

  def putWithParent[U: c.WeakTypeTag](c: Context { type PrefixType = GAEDatastore})
                   (obj: c.Expr[U], key: c.Expr[Key])(clazz: c.Expr[ClassTag[U]]): c.Expr[Unit] = {
    import c.universe._
    import scalads.macroimpls.Serializer.serializeImpl


    val deserializer = serializeImpl(c)(obj, c.Expr[Writer[Entity]](Ident(newTermName("writer"))))
    reify {
      c.prefix.splice.putRaw(clazz.splice, key.splice)( writer => deserializer.splice)
    }
  }
}
