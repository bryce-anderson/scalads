package util

import language.experimental.macros

import com.google.appengine.api.datastore.{Query => GQuery}
import com.google.appengine.api.datastore.Query.{Filter, FilterOperator, CompositeFilterOperator}
import macroimpls.QueryMacros
import com.google.appengine.api.datastore

/**
 * @author Bryce Anderson
 *         Created on 5/30/13
 */

trait Query[U] {

  protected def gQuery: GQuery  // The google api query backing this wrapper

  def filter(f: U => Boolean): Query[U] = macro QueryMacros.filterImpl[U]

  def addFilter(filter: Filter): FilteredQuery[U] = new FilteredQuery[U](filter, gQuery)

  def withParent(parent: U with EntityBacker): Query[U] = ??? // gQuery.setAncestor(parent.ds_key)

  def toList: List[U with EntityBacker] = ???
}

class FilteredQuery[U] private[util](filter: Filter, val gQuery: GQuery) extends Query[U] {

  def sortAscBy(f: U => Any): FilteredQuery[U] = ???

  def sortDecBy(f: U => Any): FilteredQuery[U] = ???
}

object Query {
  def apply[U] = new Query[U] {
    protected def gQuery: GQuery = ??? // new GQuery(U)
  }

}

