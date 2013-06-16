package scalads.mongodb

import scalads.core.{Filter, SortDirection, Projection, Query}
import com.mongodb.DBObject
import scalads.AbstractDatastore

/**
 * @author Bryce Anderson
 *         Created on 6/15/13
 */


class MongoQuery[U](val ds: MongoDatastore) extends Query[U, DBObject] { self =>

  type Repr = MongoQuery[U]

  def setFilter(filter: Filter): MongoQuery[U] = ???

  def sortBy(field: String, dir: SortDirection): MongoQuery[U] = ???

  def addProjection(proj: Projection): MongoQuery[U] = ???

  def runQuery: Iterator[DBObject] = {
    val it = ds.coll.find(null).iterator()
    new Iterator[DBObject] {
      def hasNext: Boolean = it.hasNext

      def next(): DBObject = it.next()
    }
  }

  private var limit = 0

  def limit(size: Int): MongoQuery[U] = {
    limit = size
    self
  }
}
