package scalads

import language.experimental.macros
import scala.reflect.macros.Context

import scala.collection.mutable.ListBuffer
import scalads.readers.ObjectReader
import scalads.macroimpls.{EntitySerializer, EntityBuilder, Serializer}


import scalads.core._
import scalads.writers.Writer
import scala.reflect.ClassTag

/**
 * @author Bryce Anderson
 *         Created on 5/31/13
 */
trait AbstractDatastore[+WriteResult, Entity] { self =>

  /** Type of the query that will be generated by the datastore
    *
    * @tparam U Type of the query that will be generated by the datastore
    */
  type QueryType[U] <: Query[U, Entity]

  protected def freshEntity(clazz: ClassTag[_]): Entity

  /** Stores or updates the entity in the data store
    *
    * @param entity native entity intended to be stored
    * @return result of storing the entity
    */
  def putEntity(entity: Entity): WriteResult

  /** Factory method for generating object readers
    *
    * @param entity datastore entity intended to be wrapped by the reader
    * @return appropriate object reader for the type of entity
    */
  def newReader(entity: Entity): ObjectReader

  /** Generates a writer which will store the data in the provided entity
    *
    * @param entity storage container for the writer to place data in
    * @return the writer that wraps the entity
    */
  def newWriter(entity: Entity): Writer[Entity]

  /** Returns a new query that will search for the objects of type U
    *
    * @param clazz typetag of the class of interest
    * @tparam U type of the entities of interest
    * @return the new query
    */
  def query[U](implicit clazz: ClassTag[U]): QueryType[U]

  def withTransaction[U](f: => U): U

  def delete(entity: Entity): Unit

  /** Creates a new entity which will replace the current one once persisted
    *
    * @param old entity that will be replaced
    * @return new entity that will replace the old one in the datastore
    */
  protected def replaceEntity(old: Entity): Entity

  def delete(entity: EntityBacker[_, Entity]):Unit = delete(entity.ds_entity)

  def update[U](it: QueryIterator[U with EntityBacker[U, Entity], Entity])(f: U => Option[U]) {
    val newEntities = new ListBuffer[Entity]
    it.foreach { i =>
      f(i).foreach{ r =>
          val newEntity = replaceEntity(i.ds_entity)
          i.ds_serialize(r, newWriter(newEntity))
          newEntities += newEntity
      }
    }
    put(newEntities.result(): Iterable[Entity])
  }

  def update[U,V](theOld: U with EntityBacker[U, Entity], theNew: U): WriteResult = {
    val writer = newWriter(replaceEntity(theOld.ds_entity))
    theOld.ds_serialize(theNew, writer)
    putEntity(writer.result)
  }

  def update[U](entity: Entity)(obj: U)(implicit serializer: EntitySerializer[U]): WriteResult = {
    val writer = newWriter(replaceEntity(entity))
    serializer.serialize(obj, writer)
    putEntity(writer.result)
  }

  def put(obj: EntityBacker[_, Entity]): WriteResult =  putEntity(obj.ds_entity)

  /** Places the collection of Entities into the data store
    *
    * @param entities The Iterable[Entity] to be persisted
    */
  def put(entities: Iterable[Entity]): Unit = entities.foreach(putEntity)

  /** Stores the provided object in the datastore
    *
    * @param obj object intended for storage
    * @tparam U static type of the object
    * @return result of the datastores write operation
    */
  def put[U](obj: U)(implicit serializer: EntitySerializer[U], clazz: ClassTag[U]): WriteResult =
    putEntity(serializer.serialize(obj, newWriter(freshEntity(clazz))).result)
}
