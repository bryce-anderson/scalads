package scalads.mongodb

/**
 * @author Bryce Anderson
 *         Created on 6/15/13
 */

import com.mongodb.{DB, WriteConcern, WriteResult}

import scala.reflect.runtime.universe.TypeTag

import scalads.Datastore
import org.bson.types.ObjectId
import scalads.core.EntityBacker

class MongoDatastore(protected[mongodb] val db: DB, concern: WriteConcern = new WriteConcern())
        extends Datastore[WriteResult, ScalaDSObject] { self =>

  type QueryType[U] = MongoQuery[U]

  type TFactory[U] = MongoTransformer[U]

  def update[U, V](theOld: U with EntityBacker[U, ScalaDSObject], theNew: U): WriteResult = {
    val writer = theOld.transformer.newWriter(replacementEntity(theOld.ds_entity))
    theOld.ds_serialize(theNew, writer)
    db.getCollection(theOld.ds_entity.collection)
      .update(theOld.ds_entity.json, writer.result.json, false, false, concern)
  }

  /** Creates a new entity which will replace the current one once persisted
    *
    * @param old entity that will be replaced
    * @return new entity that will replace the old one in the datastore
    */
  def replacementEntity(old: ScalaDSObject): ScalaDSObject = old.getReplacement()

  /** Stores or updates the entity in the data store
    *
    * @param entity native entity intended to be stored
    * @return result of storing the entity
    */
  def putEntity(entity: ScalaDSObject): WriteResult = {
    entity.json.get(MongoDatastore.id) match {
      case id: ObjectId =>
        db.getCollection(entity.collection).update(replacementEntity(entity).json, entity.json, true, false, concern)

      case null => db.getCollection(entity.collection).insert(entity.json, concern)
    }
  }

  /** Returns a new query that will search for the objects of type U
    *
    * @param trans transformer for the type of interest
    * @tparam U type of the entities of interest
    * @return the new query
    */
  def query[U](implicit trans: MongoTransformer[U]): MongoQuery[U] = new MongoQuery[U](self, trans)

  def delete(entity: ScalaDSObject) { db.getCollection(entity.collection).remove(entity.json, concern) }
}

object MongoDatastore {
  def apply(db: DB, concern: WriteConcern = new WriteConcern()) = new MongoDatastore(db, concern)

  private[mongodb] val id = "_id"

  def collectionName[U](implicit typeTag: TypeTag[U]) =
    scalads.util.AnnotationHelpers.getName(typeTag).replace('.','_')
}
