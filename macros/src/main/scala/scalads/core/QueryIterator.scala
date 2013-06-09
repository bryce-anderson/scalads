package scalads
package core

import com.google.appengine.api.datastore.{QueryResultIterator, Index}

import scala.collection.JavaConverters._
import scalads.readers.{GAEObjectReader, ObjectReader}

/**
 * @author Bryce Anderson
 *         Created on 6/1/13
 */

class QueryIterator[+A]
    (it: QueryResultIterator[Entity],
     ds: Datastore,
     val deserializer: (Datastore, ObjectReader) => A) extends Iterator[A] {

  def getCursor(): String = it.getCursor.toWebSafeString

  def remove() = it.remove()

  def indexList: List[Index] = it.getIndexList.asScala.toList

  def nextEntity(): Entity = it.next()

  def nextWithEntity(): (Entity, A) = {
    val entity = nextEntity()
    (entity, deserializer(ds, new GAEObjectReader(entity, "")))
  }

  override def map[B](f: A => B) = new QueryIterator[B](it, ds, (ds, e) => f(deserializer(ds, e)))

  override def hasNext(): Boolean = it.hasNext

  override def next(): A = deserializer(ds, new GAEObjectReader(nextEntity(), ""))
}
