package scalads.core

/**
 * @author Bryce Anderson
 *         Created on 6/9/13
 */

import com.google.appengine.api.datastore.{Entity, QueryResultIterator}
import scalads.readers.ObjectReader
import scalads.AbstractDatastore
import scalads.appengine.readers.GAEObjectReader

class GAEQueryIterator[+A](it: QueryResultIterator[Entity],
                          val ds: AbstractDatastore,
                           val deserializer: (AbstractDatastore, ObjectReader) => A) extends QueryIterator[A, Entity] {

  def hasNext: Boolean = it.hasNext

  def next() = deserializer(ds, newReader(nextEntity()))

  def nextEntity(): Entity = it.next()

  private[scalads] def newReader(entity: Entity) = GAEObjectReader(entity)
}
