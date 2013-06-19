package scalads
package appengine

/**
 * @author Bryce Anderson
 *         Created on 6/9/13
 */

import language.experimental.macros

import com.google.appengine.api.datastore.{Query => GQuery, Entity => GEntity, FetchOptions, Cursor, Key, PropertyProjection}
import com.google.appengine.api.datastore.Query.{Filter => GFilter, CompositeFilterOperator, FilterPredicate, SortDirection}

import scala.collection.JavaConverters._

import scalads.core.{Query, Operation, CompositeFilter, Projection, JoinOperation, Filter, SingleFilter}

/**
 * @author Bryce Anderson
 *         Created on 5/30/13
 */

class GAEQuery[U] private(val ds: GAEDatastore,
                    private val gQuery: GQuery,
                    protected val transformer: GAETransformer[U],
                    private val fetchOptions: FetchOptions)
                                extends Query[U, GEntity] { self =>

  def this(ds: GAEDatastore, gQuery: GQuery, transformer: GAETransformer[U]) =
    this(ds, gQuery, transformer, FetchOptions.Builder.withDefaults())

  type Repr = GAEQuery[U]


  override def limit(size: Int) = new GAEQuery[U](ds, gQuery, transformer, fetchOptions.limit(size))

  def withEndCursor(offset: String) =
    new GAEQuery[U](ds, gQuery, transformer, fetchOptions.endCursor(Cursor.fromWebSafeString(offset)))

  def withStartCursor(offset: String) =
    new GAEQuery[U](ds, gQuery, transformer, fetchOptions.startCursor(Cursor.fromWebSafeString(offset)))

  override def runQuery: Iterator[GEntity] = {
    val result = ds.collection.prepare(gQuery).asQueryResultIterator(fetchOptions).asScala
    result
  }

  override def setFilter(filter: Filter): Repr = {
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

    new GAEQuery[U](ds, gQuery.setFilter(walk(filter)), transformer, fetchOptions)
  }

  def withParent(key: Key): Repr = new GAEQuery[U](ds, gQuery.setAncestor(key), transformer, fetchOptions)

  /** method to add the intended projections. Intended to be called immediately before mapIterator by the project macro
    *
    * @param projs the projections to add
    * @return the query with the applied projection.
    */
  override def addProjections(projs: List[Projection]): GAEQuery[U] = {
    // Search for blacklisted types
    projs.foreach { p =>
      if (GAEQuery.projectionsBlackList.contains(p.clazz))
        return self
    }

    var projections: List[Projection] = Nil

    // Make the projections
    val newQuery = projs.foldLeft(gQuery){ (q, proj) =>
      if (!projections.contains(proj)) {
        projections = proj::projections
        q.addProjection(new PropertyProjection(proj.path.mkString("."),
          GAEQuery.clazzMap.get(proj.clazz).getOrElse(proj.clazz)))
      } else q
    }
    new GAEQuery[U](ds, newQuery, transformer, fetchOptions)
  }

  override def sortBy(field: Projection, dir: core.SortDirection): GAEQuery[U] = {
    import core.{SortDirection => SDir}
    val newQuery = gQuery.addSort(field.path.mkString("."), dir match {
      case SDir.ASC => SortDirection.ASCENDING
      case SDir.DSC => SortDirection.DESCENDING
    })
    new GAEQuery[U](ds, newQuery, transformer, fetchOptions)
  }
}

object GAEQuery {
  private[GAEQuery] val clazzMap: Map[Class[_], Class[_]] = Map(
    classOf[Int] -> classOf[java.lang.Integer],
    classOf[Long] -> classOf[java.lang.Long],
    classOf[Double] -> classOf[java.lang.Double],
    classOf[Float] -> classOf[java.lang.Float]
  )

  private[GAEQuery] val projectionsBlackList: List[Class[_]] =
      classOf[List[_]]::
      classOf[Map[_,_]]::Nil
}


