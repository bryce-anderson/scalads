package util

import com.google.appengine.api.datastore.{Entity, QueryResultIterator, Index}

import scala.collection.JavaConverters._

/**
 * @author Bryce Anderson
 *         Created on 6/1/13
 */

class QueryIterator[+U <: EntityBacker](it: QueryResultIterator[Entity], f: Entity => U) extends Iterator[U] {

  def getCursor(): String = it.getCursor.toWebSafeString

  def remove() = it.remove()

  def indexList: List[Index] = it.getIndexList.asScala.toList

  override def hasNext(): Boolean = it.hasNext

  override def next(): U = f(it.next())
}
