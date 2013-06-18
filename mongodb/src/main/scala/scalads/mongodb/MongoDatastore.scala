package scalads.mongodb

/**
 * @author Bryce Anderson
 *         Created on 6/15/13
 */

import com.mongodb._

import scalads.AbstractDatastore
import scala.reflect.runtime.universe.TypeTag
import scalads.writers.Writer
import scalads.readers.ObjectReader
import org.bson.types.ObjectId
import scalads.core.EntityBacker
import scalads.mongodb.readers.BsonObjectReader
import scalads.mongodb.writers.MongoWriter

class MongoDatastore(protected[mongodb] val collection: DBCollection, concern: WriteConcern = new WriteConcern())
        extends AbstractDatastore[WriteResult, DBObject] { self =>

  type QueryType[U] = MongoQuery[U]

  def update[U, V](theOld: U with EntityBacker[U, DBObject], theNew: U): WriteResult = {
    val writer = newWriter(replacementEntity(theOld.ds_entity))
    theOld.ds_serialize(theNew, writer)
    collection.update(theOld.ds_entity, writer.result, false, false, concern)
  }

  /** Creates a new entity which will replace the current one once persisted
    *
    * @param old entity that will be replaced
    * @return new entity that will replace the old one in the datastore
    */
  protected def replacementEntity(old: DBObject): DBObject = old.get(MongoDatastore.id) match {
    case null => sys.error("Cannot replace entity: doesn't have key.")
    case id: ObjectId => new BasicDBObject().append(MongoDatastore.id, id)
  }

  protected def freshEntity(tpe: TypeTag[_]): DBObject = {
    import scalads.util.AnnotationHelpers.getName
    new BasicDBObject(MongoDatastore.dbTypeField, getName(tpe))
  }

  /** Stores or updates the entity in the data store
    *
    * @param entity native entity intended to be stored
    * @return result of storing the entity
    */
  def putEntity(entity: DBObject): WriteResult = {
    entity.get(MongoDatastore.id) match {
      case id: ObjectId =>
        collection.update(replacementEntity(entity), entity, true, false, concern)

      case null => collection.insert(entity, concern)
    }
  }

  /** Factory method for generating object readers
    *
    * @param entity datastore entity intended to be wrapped by the reader
    * @return appropriate object reader for the type of entity
    */
  def newReader(entity: DBObject): ObjectReader = new BsonObjectReader(entity)

  /** Generates a writer which will store the data in the provided entity
    *
    * @param entity storage container for the writer to place data in
    * @return the writer that wraps the entity
    */
  def newWriter(entity: DBObject): Writer[DBObject] = new MongoWriter(entity)

  /** Returns a new query that will search for the objects of type U
    *
    * @param tpeTg typetag of the class of interest
    * @tparam U type of the entities of interest
    * @return the new query
    */
  def query[U](implicit tpeTg: TypeTag[U]): MongoQuery[U] = new MongoQuery[U](self, tpeTg)

  def delete(entity: DBObject) { collection.remove(entity, concern) }
}

object MongoDatastore {
  def apply(coll: DBCollection, concern: WriteConcern = new WriteConcern()) = new MongoDatastore(coll, concern)

  private[mongodb] val dbTypeField = "ds_type"

  private[mongodb] val id = "_id"
}
