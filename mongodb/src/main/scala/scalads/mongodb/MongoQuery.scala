package scalads.mongodb

import scalads.core._

import reactivemongo.bson._

import scalads.core.SortDirection.{ASC, DSC}

import scala.concurrent.ExecutionContext
import scala.Some
import scalads.core.Projection
import scalads.core.CompositeFilter
import reactivemongo.api.collections.default.BSONCollection
import scalads.core.SingleFilter
import scala.concurrent.duration.Duration
import scalads.readers.ObjectReader

/**
 * @author Bryce Anderson
 *         Created on 6/15/13
 */


class MongoQuery[U] private(val ds: MongoDatastore,
                            val transformer: MongoTransformer[U],
                            maxResults: Int,
                            filters: List[Filter],
                            sorts: List[BSONDocument])
                           (implicit ec: ExecutionContext) extends Query[U, ScalaDSObject] { self =>

  type Repr = MongoQuery[U]

  private val waitTime = Duration.Inf

  // Generates a fresh query
  def this(ds: MongoDatastore, tpe: MongoTransformer[U])(implicit ec: ExecutionContext) = this(ds, tpe, 0, Nil, Nil)(ec)

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
    new MongoQuery[U](ds, transformer, maxResults, filter::filters, sorts)

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
    new MongoQuery[U](ds, transformer, maxResults, filters, obj::sorts)
  }

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

  private def buildProjection(projs: List[Projection]): Option[BSONDocument] = if (projs.isEmpty) {
    Some(BSONDocument(projs.map{ p => (p.path.head, BSONInteger(1))}))
  } else None

  private def getQueryBuilder(projection: Option[BSONDocument]) = {

    val grandFilter: BSONDocument = { // Make the filters
    val newFilters = filters.map(filterwalk)
      newFilters match {
        case Nil        => BSONDocument()
        case f::Nil     => f
        case _          =>  // Join all the sub filters with an and operation
          BSONDocument("$and"-> BSONArray(newFilters))
      }
    }

    // Run the query, add the limit, and add the sort directions
    val coll = ds.db[BSONCollection](transformer.typeName)
    val it = sorts.foldRight{
      projection.fold(coll.find(grandFilter))(coll.find(grandFilter, _))
    }((s, it) => it.sort(s))

    it
  }

  def projectAndMap[T](projs: List[Projection], f: (DS, ObjectReader) => T): MongoIterator[T] = {
    val proj = buildProjection(projs)
    new MongoIterator[T](
      getQueryBuilder(proj).cursor,
      transformer.typeName,
      ec,
      d => f(ds, transformer.newReader(transformer.wrapDocument(d))),
      maxResults
    )
  }

  def runQuery = getIterator().map(_.ds_entity)

  override def getIterator(): MongoIterator[U with EntityBacker[U, ScalaDSObject]] = {
    new MongoIterator[U with EntityBacker[U, ScalaDSObject]](
      getQueryBuilder(None).cursor,
      transformer.typeName,
      ec,
      d => transformer.deserialize(ds, transformer.wrapDocument(d)),
      maxResults
    )
  }

  def limit(size: Int): MongoQuery[U] =
    new MongoQuery[U](ds, transformer, size, filters, sorts)
}
