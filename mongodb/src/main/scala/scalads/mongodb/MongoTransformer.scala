package scalads.mongodb

import scalads.core.Transformer
import com.mongodb.{BasicDBObject, DBObject}
import scalads.writers.Writer
import scalads.readers.ObjectReader

import scala.reflect.runtime.universe.TypeTag
import scalads.mongodb.readers.BsonObjectReader
import scalads.mongodb.writers.MongoWriter
import scalads.macroimpls.{EntitySerializer, EntityBuilder}

/**
 * @author Bryce Anderson
 *         Created on 6/18/13
 */
trait MongoTransformer[U] extends Transformer[U, DBObject] {
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

   def freshEntity(): DBObject = {
    import scalads.util.AnnotationHelpers.getName
    new BasicDBObject(MongoDatastore.dbTypeField, getName(typeTag))
  }

  def typeName = scalads.util.AnnotationHelpers.getName(typeTag)
}

object MongoTransformer {
  implicit def getTrans[U](implicit des: EntityBuilder[U, DBObject], ser: EntitySerializer[U], tag: TypeTag[U]) =
  new MongoTransformer[U] {
    def serializer = ser

    def deserializer = des

    def typeTag = tag
  }
}
