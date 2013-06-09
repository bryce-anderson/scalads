package scalads
package core

import language.experimental.macros

import macroimpls.QueryMacros
import scalads.AbstractDatastore
import scalads.readers.{ObjectReader, Reader}

/**
 * @author Bryce Anderson
 *         Created on 5/30/13
 */

trait AbstractQuery[U] { self =>

}

trait Query[U, E] { self =>

  def ds: AbstractDatastore

  def deserializer: (AbstractDatastore, ObjectReader) => U with EntityBacker[U]

  def limit(size: Int): this.type

  def getIterator: QueryIterator[U with EntityBacker[U, E]] = new QueryIterator(runQuery, ds, deserializer)

  def mapIterator[T](f: (AbstractDatastore[_], ObjectReader) => T): QueryIterator[T] = new QueryIterator(runQuery, ds, f)

  def mapIterator[T](f: ObjectReader => T): QueryIterator[T] = mapIterator( (_, r) => f(r) )

  def runQuery: ???

  def update(f: U => Option[U]) = ds.update(getIterator)(f)

  def setFilter(filter: Filter): this.type

  def sortBy(field: String, dir: SortDir): this.type

  def addProjection(proj: Projection): self.type

  // Macro impls

  def project[R](f: U => R): QueryIterator[R] =     macro QueryMacros.project[U, R]

  def sortAsc(f: U => Any): Query[U] =            macro QueryMacros.sortImplAsc[U]

  def sortDec(f: U => Any): Query[U] =            macro QueryMacros.sortImplDesc[U]

  def filter(f: U => Boolean): Query[U] =           macro QueryMacros.filterImpl[U]
}

