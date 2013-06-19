package scalads.appengine

import com.google.appengine.api.datastore.Entity
import scalads.core.Transformer
import scalads.appengine.readers.GAEObjectReader
import scalads.appengine.writers.GAEWriter
import scalads.macroimpls.{EntityBuilder, EntitySerializer}

import scala.reflect.runtime.universe.TypeTag

/**
 * @author Bryce Anderson
 *         Created on 6/18/13
 */
trait GAETransformer[U] extends Transformer[U, Entity] {
  /** Factory method for generating object readers
    *
    * @param entity datastore entity intended to be wrapped by the reader
    * @return appropriate object reader for the type of entity
    */
  def newReader(entity: Entity): GAEObjectReader = new GAEObjectReader(entity, "")

  /** Generates a writer which will store the data in the provided entity
    *
    * @param entity storage container for the writer to place data in
    * @return the writer that wraps the entity
    */
  def newWriter(entity: Entity) = new GAEWriter(entity)

  def getName() = scalads.util.AnnotationHelpers.getName(typeTag)
}

object GAETransformer {
  implicit def newTransformer[U]
  (implicit tag: TypeTag[U], des: EntityBuilder[U, Entity], ser: EntitySerializer[U]): GAETransformer[U] =
    new GAETransformer[U] {

      def freshEntity(): Entity = new Entity(scalads.util.AnnotationHelpers.getName(tag))

      def serializer: EntitySerializer[U] = ser

      def deserializer: EntityBuilder[U, Entity] = des

      def typeTag = tag
    }
}
