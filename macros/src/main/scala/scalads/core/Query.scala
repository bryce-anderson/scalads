package scalads
package core

import language.experimental.macros

import scalads.macroimpls.{EntityBuilder, QueryMacros}
import scalads.AbstractDatastore
import scalads.readers.ObjectReader

/**
 * @author Bryce Anderson
 *         Created on 5/30/13
 */

trait Query[U, E] { self =>

  type Repr <: Query[U, E]

  type DS = AbstractDatastore[_, E]

  def ds: DS

  def setFilter(filter: Filter): Repr

  def sortBy(field: String, dir: SortDirection): Repr

  def addProjection(proj: Projection): Repr

  def runQuery: Iterator[E]

  def limit(size: Int): Repr

  def getIterator(implicit deserializer: EntityBuilder[U, E]): QueryIterator[U with EntityBacker[U, E], E] = QueryIterator(ds, runQuery){ (ds, reader) =>
    deserializer.deserialize(ds, reader)
  }

  def mapIterator[T](f: (DS, ObjectReader) => T): QueryIterator[T, E] = {

    val it = runQuery

    new QueryIterator[T, E]{
      def hasNext: Boolean = it.hasNext
      val ds: DS = self.ds
      val deserializer: (DS, ObjectReader) => T = f
      def nextEntity(): E = it.next()
    }
  }

  def mapIterator[T](f: ObjectReader => T): QueryIterator[T, E] = mapIterator( (_, r) => f(r) )

  def project[R](f: U => R): QueryIterator[R, E] =     macro QueryMacros.project[U, R, E]

  def sortAsc(f: U => Any): Repr =                     macro QueryMacros.sortImplAsc[U, Repr]

  def sortDec(f: U => Any): Repr =                     macro QueryMacros.sortImplDesc[U, Repr]

  def filter(f: U => Boolean): Repr  =                 macro QueryMacros.filterImpl[U, Repr]
}

