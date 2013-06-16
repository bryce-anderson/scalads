package scalads.appengine

import scala.collection.JavaConverters._

import scalads.AbstractDatastore
import scalads.writers.Writer
import scalads.appengine.readers.GAEObjectReader
import scalads.appengine.writers.GAEWriter

import com.google.appengine.api.datastore.{Entity, Key,
                      DatastoreServiceFactory, DatastoreService, Query => GQuery}
import scala.reflect.ClassTag
import scalads.macroimpls.EntitySerializer

/**
 * @author Bryce Anderson
 *         Created on 6/9/13
 */
class GAEDatastore(val svc: DatastoreService) extends AbstractDatastore[Key, Entity] { self =>

  protected def replaceEntity(old: Entity): Entity = new Entity(old.getKey)

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

  protected def freshEntity(clazz: ClassTag[_]): Entity = new Entity(clazz.runtimeClass.getName())

  def query[U](implicit clazz: ClassTag[U]): QueryType[U] = {
    new GAEQuery[U](self, new GQuery(clazz.runtimeClass.getName))
  }

  /** Method overload to let GAEDatastore set parents
    *
    * @param obj the object you want to store
    * @param parent key of the parent object
    * @tparam U type of the object you want to store
    * @return key of the newly persisted entity
    */
  def put[U](obj: U, parent: Key = null)(implicit serializer: EntitySerializer[U], clazz: ClassTag[U]): Key = {
    val writer = newWriter(new Entity(clazz.runtimeClass.getName, parent))
    putEntity(serializer.serialize(obj, writer).result)
  }

}

object GAEDatastore {
  def getDatastoreService() = new GAEDatastore(DatastoreServiceFactory.getDatastoreService)

}
