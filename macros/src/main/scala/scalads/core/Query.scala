package scalads
package core

import language.experimental.macros

import com.google.appengine.api.datastore.{Query => GQuery, FetchOptions, Cursor, Projection}
import com.google.appengine.api.datastore.Query.{SortDirection, Filter}
import macroimpls.QueryMacros
import scalads.Datastore
import scalads.readers.{ObjectReader, Reader}

/**
 * @author Bryce Anderson
 *         Created on 5/30/13
 */

class Query[U](ds: Datastore, gQuery: GQuery, deserializer: (Datastore, ObjectReader) => U with EntityBacker[U]) { self =>

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

  def getIterator: QueryIterator[U with EntityBacker[U]] = new QueryIterator(runQuery, ds, deserializer)

  def mapIterator[T](f: (Datastore, ObjectReader) => T): QueryIterator[T] = new QueryIterator(runQuery, ds, f)

  def mapIterator[T](f: ObjectReader => T): QueryIterator[T] = mapIterator( (_, r) => f(r) )

  def runQuery = ds.ds.prepare(gQuery).asQueryResultIterator(fetchOptions)

  def update(f: U => Option[U]) = ds.update(getIterator)(f)

  def setFilter(filter: Filter): Query[U] = new Query[U](ds, gQuery.setFilter(filter), deserializer)

  def withParent(key: Key) = new Query[U](ds, gQuery.setAncestor(key), deserializer)

  def withParent(parent: U with EntityBacker[U]): Query[U] = withParent(parent.ds_key)

  def sortBy(field: String, dir: SortDirection) = new Query[U](ds, gQuery.addSort(field, dir), deserializer)

  private var projections: List[String] = Nil

  def addProjection(proj: Projection): self.type = {
    if (!projections.contains(proj.getName)) {
      projections = proj.getName::projections
      gQuery.addProjection(proj)
    }
    self
  }

  // Macro impls

  def project[R](f: U => R): QueryIterator[R] =     macro QueryMacros.project[U, R]

  def sortAsc(f: U => Any): Query[U] =            macro QueryMacros.sortImplAsc[U]

  def sortDec(f: U => Any): Query[U] =            macro QueryMacros.sortImplDesc[U]

  def filter(f: U => Boolean): Query[U] =           macro QueryMacros.filterImpl[U]
}

