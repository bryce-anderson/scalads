package scalads
package core

import language.experimental.macros

import com.google.appengine.api.datastore.{Query => GQuery, FetchOptions, Cursor, Projection}
import com.google.appengine.api.datastore.Query.{SortDirection, Filter}
import macroimpls.QueryMacros
import scalads.Datastore

/**
 * @author Bryce Anderson
 *         Created on 5/30/13
 */

class Query[U](ds: Datastore, gQuery: GQuery, deserializer: Entity => U with EntityBacker[U]) { self =>

  protected val fetchOptions = FetchOptions.Builder.withDefaults()

  def limit(size: Int) = new Query[U](ds, gQuery, deserializer) {
    override val fetchOptions = self.fetchOptions.limit(size)
  }

  def withEndCursor(offset: String) = new Query[U](ds, gQuery, deserializer) {
    override val fetchOptions = self.fetchOptions.endCursor(Cursor.fromWebSafeString(offset))
  }

  def withStartCursor(offset: String) = new Query[U](ds, gQuery, deserializer) {
    override val fetchOptions = self.fetchOptions.startCursor(Cursor.fromWebSafeString(offset))
  }

  def getIterator: QueryIterator[U with EntityBacker[U]] = mapIterator(deserializer)

  def mapIterator[R](f: Entity => R): QueryIterator[R] = new QueryIterator(runQuery, f)

  def runQuery = ds.ds.prepare(gQuery).asQueryResultIterator(fetchOptions)

  def update(f: U => Option[U]) = ds.update(getIterator)(f)

  def setFilter(filter: Filter): Query[U] = new Query[U](ds, gQuery.setFilter(filter), deserializer)

  def withParent(key: Key) = new Query[U](ds, gQuery.setAncestor(key), deserializer)

  def withParent(parent: U with EntityBacker[U]): Query[U] = withParent(parent.ds_key)

  def sortBy(field: String, dir: SortDirection) = new Query[U](ds, gQuery.addSort(field, dir), deserializer)

  def addProjection(proj: Projection): self.type = { gQuery.addProjection(proj); self }

  // Macro impls

  def project[R](f: U => R): QueryIterator[R] =     macro QueryMacros.project[U, R]

  def sortAsc(f: U => Any): Query[U] =            macro QueryMacros.sortImplAsc[U]

  def sortDec(f: U => Any): Query[U] =            macro QueryMacros.sortImplDesc[U]

  def filter(f: U => Boolean): Query[U] =           macro QueryMacros.filterImpl[U]
}

