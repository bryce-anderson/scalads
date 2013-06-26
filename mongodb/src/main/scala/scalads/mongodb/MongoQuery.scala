package scalads.mongodb

import scalads.core._

import reactivemongo.bson._

import scalads.core.SortDirection.{ASC, DSC}

import scalads.util.AnnotationHelpers._
import scalads.core.Projection
import scalads.core.CompositeFilter
import scalads.core.SingleFilter
import reactivemongo.api.collections.default.BSONCollection
import play.api.libs.iteratee._
import scala.concurrent.{Promise, Await, Future, ExecutionContext}
import scala.Some
import scalads.core.Projection
import scalads.core.CompositeFilter
import reactivemongo.api.collections.default.BSONCollection
import scalads.core.SingleFilter
import reactivemongo.api.Cursor
import scala.util.Success
import scala.concurrent.duration.Duration

/**
 * @author Bryce Anderson
 *         Created on 6/15/13
 */


class MongoQuery[U] private(val ds: MongoDatastore,
                            val transformer: MongoTransformer[U],
                            maxResults: Int,
                            filters: List[Filter],
                            sorts: List[BSONDocument],
                            projections: List[Projection])
                           (implicit ec: ExecutionContext) extends Query[U, ScalaDSObject] { self =>

  type Repr = MongoQuery[U]

  private val waitTime = Duration.Inf

  // Generates a fresh query
  def this(ds: MongoDatastore, tpe: MongoTransformer[U])(implicit ec: ExecutionContext) = this(ds, tpe, 0, Nil, Nil, Nil)(ec)

  private def makePath(lst: List[String], lastOp: (String) => BSONDocument): BSONDocument = lst match {
    case last::Nil => lastOp(last)
    case h::t => BSONDocument(h -> makePath(t, lastOp))
  }

  /** Generated a new query that will filter the results based on the filter
    *
    * @param filter filter to be applied to the query
    * @return new query with the filter applied
    */
  def setFilter(filter: Filter): MongoQuery[U] =
    new MongoQuery[U](ds, transformer, maxResults, filter::filters, sorts, projections)

  /** Sort the results based on the projection and sorting direction
    *
    * @param field Projection representing the field to sort by
    * @param dir direction with which to sort
    * @return new query which will sort the result by the field specified
    */
  def sortBy(field: Projection, dir: SortDirection): MongoQuery[U] = {
    val order = dir match {
      case ASC =>  1
      case DSC => -1
    }
    val obj = makePath(field.path, str => BSONDocument(str -> order))
    new MongoQuery[U](ds, transformer, maxResults, filters, obj::sorts, projections)
  }

  /** method to add the intended projections. Intended to be called immediately before mapIterator by the project macro
    *
    * @param projs the projections to add
    * @return the query with the applied projection.
    */
  protected def addProjections(projs: List[Projection]): MongoQuery[U] =
    new MongoQuery[U](ds, transformer, maxResults, filters, sorts, projs:::projections)


  // generates the DBObject for a filter
  private def filterwalk(f: Filter): BSONDocument = f match {
    case f: SingleFilter =>
      val op = f.op match {
        case Operation.EQ => "$eq"
        case Operation.LT => "$lt"
        case Operation.GT => "$gt"
        case Operation.LE => "$lte"
        case Operation.GE => "$gte"
        case Operation.NE => "$ne"
      }

      makePath(f.axis.path, key => BSONDocument(key ->  BSONDocument(op -> MongoDatastore.mongoHandle(f.value))))

    case CompositeFilter(f1, f2, JoinOperation.AND) =>
      val lst = BSONArray( filterwalk(f1), filterwalk(f2) )
      BSONDocument("$and" -> lst)

    case CompositeFilter(f1, f2, JoinOperation.OR) =>
      val lst = BSONArray( filterwalk(f1), filterwalk(f2) )
      BSONDocument("$or" -> lst)
  }

  private def buildProjection(projs: List[Projection]): BSONDocument = {
    BSONDocument(projs.map{ p => (p.path.head, BSONInteger(1))})
  }

  private def getCursor: Cursor[BSONDocument] = {
    // Make the filters
    val grandFilter: BSONDocument = {
      val newFilters = filters.map(filterwalk)
      newFilters match {
        case Nil        => BSONDocument()
        case f::Nil     => f
        case _          =>  // Join all the sub filters with an and operation
          BSONDocument("$and"-> BSONArray(newFilters))
      }
    }

    val grandProjection: Option[BSONDocument] = if (!projections.isEmpty) {
      Some(buildProjection(projections))
    } else None

    // Run the query, add the limit, and add the sort directions
    val coll = ds.db[BSONCollection](transformer.typeName)
    val it = sorts.foldRight{
      grandProjection.fold(coll.find(grandFilter))(coll.find(grandFilter, _))
    }((s, it) => it.sort(s))

    it.cursor
  }

  def runQuery: Iterator[ScalaDSObject] = {
    var cursor = getCursor

    def refreshCursor(): Cursor[BSONDocument] = {
      while(!cursor.iterator.hasNext && cursor.hasNext)
        cursor = Await.result(cursor.next, waitTime)
      cursor
    }

    var resultCount = 0

    new Iterator[ScalaDSObject] {
      def hasNext: Boolean = {
        if (maxResults > 0 && resultCount == maxResults) return false
        if (cursor.iterator.hasNext) return true
        refreshCursor()
        cursor.iterator.hasNext
      }

      def next(): ScalaDSObject = {
        resultCount += 1
        if (maxResults > 0 && resultCount > maxResults) sys.error(s"Iterator is past the requested results: MaxResults: $maxResults")

        if (cursor.iterator.hasNext) return new ScalaDSObject(transformer.typeName, cursor.iterator.next())
        refreshCursor()
        new ScalaDSObject(transformer.typeName, cursor.iterator.next())
      }
    }
  }

  def limit(size: Int): MongoQuery[U] =
    new MongoQuery[U](ds, transformer, size, filters, sorts, projections)
}
