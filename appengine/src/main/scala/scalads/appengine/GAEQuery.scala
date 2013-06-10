package scalads.appengine

/**
 * @author Bryce Anderson
 *         Created on 6/9/13
 */

import language.experimental.macros

import com.google.appengine.api.datastore.{Query => GQuery, Entity => GEntity, FetchOptions, Cursor, Key, PropertyProjection}
import com.google.appengine.api.datastore.Query.{Filter => GFilter, CompositeFilterOperator, FilterPredicate, SortDirection}
import scalads.readers.{ObjectReader, Reader}
import scalads.core._

import scala.collection.JavaConverters._
import scalads.{core, AbstractDatastore}
import scalads.core.SingleFilter

/**
 * @author Bryce Anderson
 *         Created on 5/30/13
 */

class GAEQuery[U](val ds: GAEDatastore,
                        private var gQuery: GQuery,
                        val deserializer: (AbstractDatastore[_, GEntity], ObjectReader) => U with EntityBacker[U, GEntity])
            extends Query[U, GEntity] { self =>

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

  def runQuery = ds.svc.prepare(gQuery).asQueryResultIterator(fetchOptions).asScala

  def setFilter(filter: Filter): this.type = {

    def walk(f: Filter): GFilter = f match {
      case f: SingleFilter =>
        val op = f.op match {
          case Operation.eq => GQuery.FilterOperator.EQUAL
          case Operation.lt => GQuery.FilterOperator.LESS_THAN
          case Operation.gt => GQuery.FilterOperator.GREATER_THAN
          case Operation.le => GQuery.FilterOperator.LESS_THAN_OR_EQUAL
          case Operation.ge => GQuery.FilterOperator.GREATER_THAN_OR_EQUAL
          case Operation.ne => GQuery.FilterOperator.NOT_EQUAL
        }
        new FilterPredicate(f.axis.path.mkString("."), op, f.value)

      case CompositeFilter(f1, f2, JoinOperation.and) => CompositeFilterOperator.and(walk(f1), walk(f2))
      case CompositeFilter(f1, f2, JoinOperation.or) =>  CompositeFilterOperator.or(walk(f1), walk(f2))
    }

    gQuery = gQuery.setFilter(walk(filter))
    self
  }

  def withParent(key: Key): this.type = {
    gQuery = gQuery.setAncestor(key)
    self
  }

  //def withParent(parent: U with EntityBacker[U, Entity]): this.type = withParent(parent.ds_key)

  private var projections: List[Projection] = Nil

  def addProjection(proj: Projection): this.type = {
    if (!projections.contains(proj)) {
      projections = proj::projections
      gQuery = gQuery.addProjection(new PropertyProjection(proj.path.mkString("."), proj.clazz.getOrElse(null)))
    }
    self
  }

  def sortBy(field: String, dir: _root_.scalads.core.SortDir): this.type = ???
}


