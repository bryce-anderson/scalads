package scalads.mongodb

import scalads.core.{Filter, SortDirection, Projection, Query}
import com.mongodb.DBObject

/**
 * @author Bryce Anderson
 *         Created on 6/15/13
 */
class MongoQuery[U] extends Query[U, DBObject] {
  type Repr = MongoQuery[U]

  def ds: MongoQuery[U]#DS = ???

  def setFilter(filter: Filter): MongoQuery[U]#Repr = ???

  def sortBy(field: String, dir: SortDirection): MongoQuery[U]#Repr = ???

  def addProjection(proj: Projection): MongoQuery[U]#Repr = ???

  def runQuery: Iterator[DBObject] = ???

  def limit(size: Int): MongoQuery[U]#Repr = ???
}
