package scalads.appengine

import scala.collection.JavaConverters._

import scalads.AbstractDatastore

import com.google.appengine.api.datastore.{Entity, Key,
                      DatastoreServiceFactory, DatastoreService, Query => GQuery}
import scala.reflect.runtime.universe.TypeTag
import scalads.core.EntityBacker

import scalads.util.AnnotationHelpers.getName

/**
 * @author Bryce Anderson
 *         Created on 6/9/13
 */
class GAEDatastore(val collection: DatastoreService) extends AbstractDatastore[Key, Entity] { self =>


  def update[U, V](theOld: U with EntityBacker[U, Entity], theNew: U): Key = {
    val writer = theOld.transformer.newWriter(replacementEntity(theOld.ds_entity))
    theOld.ds_serialize(theNew, writer)
    putEntity(writer.result)
  }

  protected def replacementEntity(old: Entity): Entity = new Entity(old.getKey)

  type QueryType[U] = GAEQuery[U]

  type TFactory[U] = GAETransformer[U]

  def withTransaction[U](f: => U): U = {
    val txn = collection.beginTransaction()
    try { val a = f; txn.commit(); a }
    finally { if(txn.isActive) txn.rollback() }
  }

  def delete(entity: Entity) {collection.delete(entity.getKey)}

  override def put(entities: Iterable[Entity]) { collection.put(entities.asJava) }

  def putEntity(entity: Entity): Key = collection.put(entity)

  def query[U](implicit transformer: GAETransformer[U]): QueryType[U] = {
    new GAEQuery[U](self, new GQuery(getName(transformer.typeTag)), transformer)
  }

  /** Method overload to let GAEDatastore set parents
    *
    * @param obj the object you want to store
    * @param parent key of the parent object
    * @tparam U type of the object you want to store
    * @return key of the newly persisted entity
    */
  def put[U](obj: U, parent: Key)(implicit transformer: GAETransformer[U], tpeTg: TypeTag[U]): Key = {
    val writer = transformer.newWriter(new Entity(getName(tpeTg), parent))
    putEntity(transformer.serializer.serialize(obj, writer).result)
  }

}

object GAEDatastore {
  def getDatastoreService() = new GAEDatastore(DatastoreServiceFactory.getDatastoreService)

}
