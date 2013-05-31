package util

import language.experimental.macros

import com.google.appengine.api.datastore.Query.{Filter, FilterOperator, CompositeFilterOperator}
import macroimpls.QueryMacros

/**
 * @author Bryce Anderson
 *         Created on 5/30/13
 */

trait Query[U] {
  def addFilter(filter: Filter): Query[U]

  def filter(f: U => Boolean): Query[U] = macro QueryMacros.filterImpl[U]

  def sortBy(f: U => AnyRef): Query[U] = ???

  def withParent(parent: U with EntityBacker): Query[U] = ???

  def toList: List[U] = ???
}

object Query {

}
