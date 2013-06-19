package scalads.core

import scalads.readers.ObjectReader
import scalads.writers.Writer
import scalads.macroimpls.{EntityBuilder, EntitySerializer}

import scala.reflect.runtime.universe.TypeTag
import scalads.AbstractDatastore

/**
 * @author Bryce Anderson
 *         Created on 6/18/13
 */
trait Transformer[U, Entity] {

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

  def freshEntity(): Entity

  def serializer: EntitySerializer[U]

  def deserializer: EntityBuilder[U, Entity]

  def typeTag: TypeTag[U]
}