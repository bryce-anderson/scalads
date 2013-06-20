package scalads.mongodb

import scalads.core._
import com.mongodb.{BasicDBList, BasicDBObject, DBObject}
import scalads.core.Projection
import scala.reflect.runtime.universe.TypeTag
import scalads.core.SortDirection.{ASC, DSC}
import scala.annotation.tailrec
import org.bson.types.BasicBSONList
import scalads.util.AnnotationHelpers._
import scala.Some
import scalads.core.Projection
import scalads.core.CompositeFilter
import scalads.core.SingleFilter

/**
 * @author Bryce Anderson
 *         Created on 6/15/13
 */


class MongoQuery[U] private(val ds: MongoDatastore,
                            val transformer: MongoTransformer[U],
                            maxResults: Int,
                            filters: List[Filter],
                            sorts: List[DBObject],
                            projections: List[Projection]) extends Query[U, ScalaDSObject] { self =>

  type Repr = MongoQuery[U]

  // Generates a fresh query
  def this(ds: MongoDatastore, tpe: MongoTransformer[U]) = this(ds, tpe, 0, Nil, Nil, Nil)

  private def makePath(lst: List[String], lastOp: (String) => DBObject): DBObject = lst match {
    case last::Nil => lastOp(last)
    case h::t => new BasicDBObject(h, makePath(t, lastOp))
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
    val obj = makePath(field.path, str => new BasicDBObject(str, order))
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
  private def filterwalk(f: Filter): DBObject = f match {
    case f: SingleFilter =>
      val op = f.op match {
        case Operation.EQ => "$eq"
        case Operation.LT => "$lt"
        case Operation.GT => "$gt"
        case Operation.LE => "$lte"
        case Operation.GE => "$gte"
        case Operation.NE => "$ne"
      }

      makePath(f.axis.path, key => new BasicDBObject(key, new BasicDBObject(op, f.value)))

    case CompositeFilter(f1, f2, JoinOperation.AND) =>
      val lst = new BasicDBList
      lst.put(0, filterwalk(f1))
      lst.put(1, filterwalk(f2))
      new BasicDBObject("$and", lst)

    case CompositeFilter(f1, f2, JoinOperation.OR) =>
      val lst = new BasicDBList
      lst.add(filterwalk(f1))
      lst.add(filterwalk(f2))
      new BasicDBObject("$or", lst)
  }

  // takes an initial object and merges the projection into the tree structure
  private def addProjection(obj: DBObject, proj: Projection): DBObject = {
    @tailrec def addFromList(obj: DBObject, path: List[String]): Unit = path match {
      case key::Nil         => obj.put(key, 1)
      case h::t             => obj.get(h) match {
        case obj: DBObject    => addFromList(obj, t)
        case null             =>
          val newObj = new BasicDBObject()
          obj.put(h, newObj)
          addFromList(newObj, t)
      }
    }
    addFromList(obj, proj.path)
    obj
  }

  def runQuery: Iterator[ScalaDSObject] = {
    // Make the filters
    val grandFilter: DBObject = {
      val newFilters = filters.map(filterwalk)
      newFilters match {
        case Nil        => null
        case f::Nil     => f
        case _          =>  // Join all the sub filters with an and operation
          val obj = new BasicBSONList()
          newFilters.zipWithIndex.foreach { case (f, i) => obj.put(i, f) }
          new BasicDBObject("$and", obj)
      }
    }

    val grandProjection: DBObject = if (!projections.isEmpty) {
        val rootObj = new BasicDBObject()
        projections.foreach(addProjection(rootObj, _))
        rootObj
    } else null

    // Run the query, add the limit, and add the sort directions
    val coll = ds.db.getCollection(transformer.typeName)
    val it = sorts.foldRight{
      coll.find(grandFilter, grandProjection).limit(maxResults)
    }((s, it) => it.sort(s))

    new Iterator[ScalaDSObject] {
      def hasNext: Boolean = it.hasNext
      def next(): ScalaDSObject = new ScalaDSObject(transformer.typeName, it.next())
    }
  }

  def limit(size: Int): MongoQuery[U] =
    new MongoQuery[U](ds, transformer, size, filters, sorts, projections)
}
