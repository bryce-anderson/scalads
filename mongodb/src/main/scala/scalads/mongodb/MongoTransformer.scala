package scalads.mongodb

import scalads.core.Transformer

import scalads.writers.Writer
import scalads.readers.ObjectReader

import scala.reflect.runtime.universe.TypeTag
import scalads.mongodb.readers.BsonObjectReader
import scalads.mongodb.writers.MongoWriter
import scalads.macroimpls.{EntitySerializer, EntityBuilder}
import reactivemongo.bson.BSONDocument

/**
 * @author Bryce Anderson
 *         Created on 6/18/13
 */
trait MongoTransformer[U] extends Transformer[U, ScalaDSObject] {
  /** Factory method for generating object readers
    *
    * @param entity datastore entity intended to be wrapped by the reader
    * @return appropriate object reader for the type of entity
    */
  def newReader(entity: ScalaDSObject): ObjectReader = new BsonObjectReader(entity.json)

  /** Generates a writer which will store the data in the provided entity
    *
    * @param entity storage container for the writer to place data in
    * @return the writer that wraps the entity
    */
  def newWriter(entity: ScalaDSObject): Writer[ScalaDSObject] = new MongoWriter(entity)

   def freshEntity(): ScalaDSObject = new ScalaDSObject(typeName)

  def typeTag: TypeTag[U]

  def wrapDocument(entity: BSONDocument) = new ScalaDSObject(typeName, entity)

  lazy val typeName: String = MongoDatastore.collectionName(typeTag)

  def getKeyString(entity: ScalaDSObject): String = entity.getIDString()
}

object MongoTransformer {
  implicit def getMongoTrans[U](implicit des: EntityBuilder[U, ScalaDSObject],
                                ser: EntitySerializer[U],
                                tag: TypeTag[U]) =
    new MongoTransformer[U] {

      def serializer = ser

      def deserializer = des

      def typeTag = tag
    }
}
