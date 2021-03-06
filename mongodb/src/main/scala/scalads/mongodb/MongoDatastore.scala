package scalads.mongodb

/**
 * @author Bryce Anderson
 *         Created on 6/15/13
 */

import reactivemongo.api.DB

import scala.reflect.runtime.universe.TypeTag

import scalads.Datastore
import scalads.core.EntityBacker
import reactivemongo.core.commands.LastError
import scala.concurrent.{ExecutionContext, Future}

import reactivemongo.bson._
import scala.Some
import reactivemongo.api.collections.default.BSONCollection
import java.util.Date
import scalads.exceptions.MappingException
import com.typesafe.config.ConfigFactory


class MongoDatastore(protected[mongodb] val db: DB)(implicit ec: ExecutionContext)
        extends Datastore[Future[LastError], ScalaDSObject] { self =>

  type QueryType[U] = MongoQuery[U]

  type TFactory[U] = MongoTransformer[U]

  def update[U, V](theOld: U with EntityBacker[U, ScalaDSObject], theNew: U): Future[LastError] = {
    val writer = theOld.transformer.newWriter(replacementEntity(theOld.ds_entity))
    theOld.ds_serialize(theNew, writer)
    db[BSONCollection](theOld.ds_entity.collection)
      .update(theOld.ds_entity.json, writer.result.json)
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
  def putEntity(entity: ScalaDSObject): Future[LastError] = {
    entity.json.get(MongoDatastore.id) match {
      case Some(id: BSONObjectID)=>
        db[BSONCollection](entity.collection).update(replacementEntity(entity).json, entity.json)

      case Some(e) => throw new MappingException(s"BSONDocument has wrong type in id field: $e")
      case None => db[BSONCollection](entity.collection).insert(entity.json)
    }
  }

  /** Returns a new query that will search for the objects of type U
    *
    * @param trans transformer for the type of interest
    * @tparam U type of the entities of interest
    * @return the new query
    */
  def query[U](implicit trans: MongoTransformer[U]): MongoQuery[U] = new MongoQuery[U](self, trans)

  def delete(entity: ScalaDSObject) { db[BSONCollection](entity.collection).remove(entity.json) }

  /** Drops the collection corresponding to the type provided
    *
    * @tparam U type of object stored in the collection
    * @return the future will be completed with an error if the collection does not exist
    */
  def drop[U: TypeTag]: Future[Boolean] = db[BSONCollection](MongoDatastore.collectionName).drop()
}

object MongoDatastore {
  def apply(db: DB)(implicit ec: ExecutionContext) = new MongoDatastore(db)(ec)

  private[mongodb] val id = "_id"

  def collectionName[U](implicit typeTag: TypeTag[U]) =
    scalads.util.AnnotationHelpers.getName(typeTag).replace('.','_')

  def mongoHandle(in: Any): BSONValue = in match {
    case b: BigInt => BSONInteger(b.intValue())
    case b: BigDecimal => BSONDouble(b.doubleValue())
    case i: Int => BSONInteger(i)
    case i: Long => BSONLong(i)
    case f: Float => BSONDouble(f)
    case d: Double => BSONDouble(d)
    case s: String => BSONString(s)
    case d: Date => BSONDateTime(d.getTime)

    case e => sys.error(s"Match error! Found type ${e.getClass}")
  }

  private[mongodb] val config = ConfigFactory.load()
}
