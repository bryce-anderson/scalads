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
trait AbstractDatastore[Key >: Null, Entity] { self =>

  type QueryType[U] <: Query[U, Entity]

  def newReader(entity: Entity): ObjectReader

  def newWriter(entity: Entity): Writer[Entity]

  def putEntity(entity: Entity): Key

  // Takes a method that will operate on the writer
  def putRaw(tpe: ClassTag[_], parent: Key)( f: Writer[Any] => Unit): Key

  def query[U](implicit clazz: ClassTag[U]): QueryType[U]

  def withTransaction[U](f: => U): U

  def delete(entity: Entity): Unit

  def delete(entity: EntityBacker[_, Entity]):Unit = delete(entity.ds_entity)

  def update[U](it: QueryIterator[U with EntityBacker[U, Entity], Entity])(f: U => Option[U]) {
    val newEntities = new ListBuffer[Entity]
    it.foreach { i =>
      f(i).foreach{ r =>
          i.ds_serialize(r, newWriter(i.ds_entity))
          newEntities += i.ds_entity
      }
    }
    put(newEntities.toList: Iterable[Entity])
  }

  def put(obj: EntityBacker[_, Entity]) {
    obj.ds_updateEntity
    putEntity(obj.ds_entity)
  }

  /** Places the collection of Entities into the data store
    *
    * @param entities The Iterable[Entity] to be persisted
    */
  def put(entities: Iterable[Entity]): Unit = entities.foreach(putEntity)

  def put[U: EntitySerializer: ClassTag](obj: U, parent: Key = null): Key = {
    putRaw(implicitly[ClassTag[U]], parent)( writer =>
      implicitly[EntitySerializer[U]].serialize(obj, writer)
    )
  }
}
