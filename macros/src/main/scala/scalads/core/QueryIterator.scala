package scalads.core

import com.google.appengine.api.datastore.{Entity, QueryResultIterator, Index}

import scala.collection.JavaConverters._

/**
 * @author Bryce Anderson
 *         Created on 6/1/13
 */

class QueryIterator[+A]
    (it: QueryResultIterator[Entity], val deserializer: Entity => A) extends Iterator[A] {

  def getCursor(): String = it.getCursor.toWebSafeString

  def remove() = it.remove()

  def indexList: List[Index] = it.getIndexList.asScala.toList

  def nextEntity(): Entity = it.next()

  def nextWithEntity(): (Entity, A) = {
    val entity = nextEntity()
    (entity, deserializer(entity))
  }

  override def map[B](f: A => B) = new QueryIterator[B](it, e => f(deserializer(e)))

  override def hasNext(): Boolean = it.hasNext

  override def next(): A = deserializer(nextEntity())
}
