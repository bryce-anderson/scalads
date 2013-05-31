package util

import language.experimental.macros

import com.google.appengine.api.datastore.{Query => GQuery, Key, DatastoreService}
import com.google.appengine.api.datastore.Query.{SortDirection, Filter, FilterOperator, CompositeFilterOperator}
import macroimpls.QueryMacros

/**
 * @author Bryce Anderson
 *         Created on 5/30/13
 */

trait Query[U] { self =>

  protected def ds: DatastoreService

  protected val gQuery: GQuery  // The google api query backing this wrapper

  def runQuery = ds.prepare(gQuery).asIterator()

  def filter(f: U => Boolean): FilteredQuery[U] = macro QueryMacros.filterImpl[U]

  def addFilter(filter: Filter): FilteredQuery[U] = new FilteredQuery[U](gQuery.setFilter(filter), ds)

  def withParent(key: Key) = new Query[U] {
    val gQuery = self.gQuery.setAncestor(key)
    def ds = self.ds
  }

  def withParent(parent: U with EntityBacker): Query[U] = withParent(parent.ds_key)

  def getIterator: Iterator[U with EntityBacker] = macro QueryMacros.getIteratorImpl[U]
}

class FilteredQuery[U] private[util](val gQuery: GQuery, val ds: DatastoreService) extends Query[U] {

  override def withParent(parent: Key) = new FilteredQuery[U](gQuery.setAncestor(parent), ds)

  override def withParent(parent: U with EntityBacker) = withParent(parent.ds_key)

  def sortAscBy(f: U => Any): FilteredQuery[U] = macro QueryMacros.sortImplAsc[U]

  def sortDecBy(f: U => Any): FilteredQuery[U] = macro QueryMacros.sortImplDesc[U]

  def sortBy(field: String, dir: SortDirection) = new FilteredQuery[U](gQuery.addSort(field, dir), ds)
}

object Query {

  def apply[U](implicit datastore: DatastoreService) = macro QueryMacros.ObjApplyImpl[U]

}

