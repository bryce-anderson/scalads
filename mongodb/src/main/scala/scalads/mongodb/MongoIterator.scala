package scalads.mongodb

import scalads.core.QueryIterator
import reactivemongo.bson.BSONDocument
import reactivemongo.api.Cursor
import scala.concurrent.{ExecutionContext, Await}
import scala.concurrent.duration.Duration
import play.api.libs.iteratee.Enumerator

/**
 * @author Bryce Anderson
 *         Created on 6/27/13
 */
class MongoIterator[T] private[mongodb](private var cursor: Cursor[BSONDocument],
                       typeName: String,
                       ec: ExecutionContext,
                       mapper: BSONDocument => T,
                       maxResults: Int = -1,
                       waitTime: Duration = Duration.Inf) extends QueryIterator[T, ScalaDSObject] {


  private def refreshCursor(): Cursor[BSONDocument] = {
    while(!cursor.iterator.hasNext && cursor.hasNext)
      cursor = Await.result(cursor.next, waitTime)
    cursor
  }

  private var resultCount = 0

  private val it = new Iterator[ScalaDSObject] {
    def hasNext: Boolean = {
      if (maxResults > 0 && resultCount == maxResults) false
      else if (cursor.iterator.hasNext) true
      else {
        refreshCursor()
        cursor.iterator.hasNext
      }
    }

    def next(): ScalaDSObject = {
      resultCount += 1
      if (maxResults > 0 && resultCount > maxResults) sys.error(s"Iterator is past the requested results: MaxResults: $maxResults")

      if (cursor.iterator.hasNext) return new ScalaDSObject(typeName, cursor.iterator.next())
      else {
        refreshCursor()
        new ScalaDSObject(typeName, cursor.iterator.next())
      }
    }
  }

  override def map[U](f: T => U): MongoIterator[U] =
    new MongoIterator[U](cursor, typeName, ec, doc => f(mapper(doc)), maxResults - resultCount, waitTime)

  def nextEntity(): ScalaDSObject = it.next()

  def next(): T = mapper(nextEntity().json)

  def hasNext: Boolean = it.hasNext

  def enumerate: Enumerator[T] = cursor.enumerate()(ec).map(mapper)

}
