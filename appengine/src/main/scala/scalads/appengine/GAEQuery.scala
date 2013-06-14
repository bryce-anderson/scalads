package scalads
package appengine

/**
 * @author Bryce Anderson
 *         Created on 6/9/13
 */

import language.experimental.macros

import com.google.appengine.api.datastore.{Query => GQuery, Entity => GEntity, FetchOptions, Cursor, Key, PropertyProjection}
import com.google.appengine.api.datastore.Query.{Filter => GFilter, CompositeFilterOperator, FilterPredicate, SortDirection}
import scalads.readers.ObjectReader


import scala.collection.JavaConverters._


import scalads.core.{EntityBacker, Query, Operation,
        CompositeFilter, Projection, JoinOperation, Filter, SingleFilter}
import scalads.macroimpls.EntityBuilder

/**
 * @author Bryce Anderson
 *         Created on 5/30/13
 */

class GAEQuery[U](val ds: GAEDatastore,
        private var gQuery: GQuery)
            extends Query[U, GEntity] { self =>

  type Repr = GAEQuery[U]

  private var fetchOptions = FetchOptions.Builder.withDefaults()


  def limit(size: Int) = {
    fetchOptions = fetchOptions.limit(size)
    self
  }

  def withEndCursor(offset: String) = {
    fetchOptions = fetchOptions.endCursor(Cursor.fromWebSafeString(offset))
    self
  }

  def withStartCursor(offset: String) = {
    fetchOptions = fetchOptions.startCursor(Cursor.fromWebSafeString(offset))
    self
  }

  def runQuery = {
    val result = ds.svc.prepare(gQuery).asQueryResultIterator(fetchOptions).asScala
    result
  }

  def setFilter(filter: Filter): Repr = {

    def walk(f: Filter): GFilter = f match {
      case f: SingleFilter =>
        val op = f.op match {
          case Operation.EQ => GQuery.FilterOperator.EQUAL
          case Operation.LT => GQuery.FilterOperator.LESS_THAN
          case Operation.GT => GQuery.FilterOperator.GREATER_THAN
          case Operation.LE => GQuery.FilterOperator.LESS_THAN_OR_EQUAL
          case Operation.GE => GQuery.FilterOperator.GREATER_THAN_OR_EQUAL
          case Operation.NE => GQuery.FilterOperator.NOT_EQUAL
        }
        new FilterPredicate(f.axis.path.mkString("."), op, f.value)

      case CompositeFilter(f1, f2, JoinOperation.AND) => CompositeFilterOperator.and(walk(f1), walk(f2))
      case CompositeFilter(f1, f2, JoinOperation.OR) =>  CompositeFilterOperator.or(walk(f1), walk(f2))
    }

    gQuery = gQuery.setFilter(walk(filter))
    self
  }

  def withParent(key: Key): Repr = {
    gQuery = gQuery.setAncestor(key)
    self
  }

  private var projections: List[Projection] = Nil

  def addProjection(proj: Projection): Repr = {
    if (!projections.contains(proj)) {
      projections = proj::projections
      gQuery = gQuery.addProjection(new PropertyProjection(proj.path.mkString("."),
        GAEQuery.clazzMap.get(proj.clazz).getOrElse(proj.clazz)))
    }
    self
  }

  def sortBy(field: String, dir: core.SortDirection): Repr = {
    import core.{SortDirection => SDir}
    gQuery = gQuery.addSort(field, dir match {
      case SDir.ASC => SortDirection.ASCENDING
      case SDir.DSC => SortDirection.DESCENDING
    })
    self
  }
}

object GAEQuery {
  private[GAEQuery] val clazzMap: Map[Class[_], Class[_]] = Map(
    classOf[Int] -> classOf[java.lang.Integer],
    classOf[Long] -> classOf[java.lang.Long],
    classOf[Double] -> classOf[java.lang.Double],
    classOf[Float] -> classOf[java.lang.Float]
  )
}


