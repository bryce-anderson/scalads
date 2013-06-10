package scalads.appengine

/**
 * @author Bryce Anderson
 *         Created on 6/9/13
 */

import com.google.appengine.api.datastore.{Entity => GEntity, QueryResultIterator}
import scalads.readers.ObjectReader
import scalads.AbstractDatastore
import scalads.appengine.readers.GAEObjectReader
import scalads.core.QueryIterator

class GAEQueryIterator[+A](it: QueryResultIterator[GEntity],
                          val ds: GAEDatastore,
                           val deserializer: (AbstractDatastore[_, GEntity], ObjectReader) => A) extends QueryIterator[A, GEntity] {

  def hasNext: Boolean = it.hasNext

  def nextEntity(): GEntity = it.next()

  private[scalads] def newReader(entity: GEntity) = GAEObjectReader(entity)
}
