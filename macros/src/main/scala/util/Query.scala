package util

import language.experimental.macros

import com.google.appengine.api.datastore.{Query => GQuery, Key, DatastoreService, FetchOptions, Cursor}
import com.google.appengine.api.datastore.Query.{SortDirection, Filter}
import macroimpls.QueryMacros

/**
 * @author Bryce Anderson
 *         Created on 5/30/13
 */

class Query[U](ds: DatastoreService, gQuery: GQuery) { self =>

  protected val fetchOptions = FetchOptions.Builder.withDefaults()

  def limit(size: Int) = new Query[U](ds, gQuery) {
    override val fetchOptions = self.fetchOptions.limit(size)
  }

  def withEndCursor(offset: String) = new Query[U](ds, gQuery) {
    override val fetchOptions = self.fetchOptions.endCursor(Cursor.fromWebSafeString(offset))
  }

  def withStartCursor(offset: String) = new Query[U](ds, gQuery) {
    override val fetchOptions = self.fetchOptions.startCursor(Cursor.fromWebSafeString(offset))
  }

  def runQuery = ds.prepare(gQuery).asQueryResultIterator(fetchOptions)

  def setFilter(filter: Filter): Query[U] = new Query[U](ds, gQuery.setFilter(filter))

  def withParent(key: Key) = new Query[U](ds, gQuery.setAncestor(key))

  def withParent(parent: U with EntityBacker[U]): Query[U] = withParent(parent.ds_key)

  def sortBy(field: String, dir: SortDirection) = new Query[U](ds, gQuery.addSort(field, dir))

  def getIterator: QueryIterator[U with EntityBacker[U]] =  macro QueryMacros.getIteratorImpl[U]

  def sortAscBy(f: U => Any): Query[U] =            macro QueryMacros.sortImplAsc[U]

  def sortDecBy(f: U => Any): Query[U] =            macro QueryMacros.sortImplDesc[U]

  def filter(f: U => Boolean): Query[U] =           macro QueryMacros.filterImpl[U]
}

