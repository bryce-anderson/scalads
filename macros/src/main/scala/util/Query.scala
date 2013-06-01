package util

import language.experimental.macros

import com.google.appengine.api.datastore.{Query => GQuery, Key, DatastoreService, FetchOptions}
import com.google.appengine.api.datastore.Query.{SortDirection, Filter}
import macroimpls.QueryMacros

/**
 * @author Bryce Anderson
 *         Created on 5/30/13
 */

class Query[U](ds: DatastoreService, gQuery: GQuery) { self =>

  def runQuery = ds.prepare(gQuery).asIterator()

  def setFilter(filter: Filter): Query[U] = new Query[U](ds, gQuery.setFilter(filter))

  def withParent(key: Key) = new Query[U](ds, gQuery.setAncestor(key))

  def withParent(parent: U with EntityBacker): Query[U] = withParent(parent.ds_key)

  def sortBy(field: String, dir: SortDirection) = new Query[U](ds, gQuery.addSort(field, dir))

  def getIterator: Iterator[U with EntityBacker] =  macro QueryMacros.getIteratorImpl[U]

  def sortAscBy(f: U => Any): Query[U] =            macro QueryMacros.sortImplAsc[U]

  def sortDecBy(f: U => Any): Query[U] =            macro QueryMacros.sortImplDesc[U]

  def filter(f: U => Boolean): Query[U] =           macro QueryMacros.filterImpl[U]

}

object Query {

  def apply[U](implicit datastore: DatastoreService) = macro QueryMacros.ObjApplyImpl[U]

}

