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

  type DS = AbstractDatastore[_, E]

  def ds: DS

  def setFilter(filter: Filter): this.type

  def sortBy(field: String, dir: SortDirection): this.type

  def addProjection(proj: Projection): this.type

  def runQuery: Iterator[E]

  def limit(size: Int): this.type

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

  def sortAsc(f: U => Any) =                           macro QueryMacros.sortImplAsc[U]

  def sortDec(f: U => Any) =                           macro QueryMacros.sortImplDesc[U]

  def filter(f: U => Boolean)  =                       macro QueryMacros.filterImpl[U]
}

