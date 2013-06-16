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

  /** Generated a new query that will filter the results based on the filter
    *
    * @param filter filter to be applied to the query
    * @return new query with the filter applied
    */
  def setFilter(filter: Filter): Repr

  /** Sort the results based on the projection and sorting direction
    *
    * @param field Projection representing the field to sort by
    * @param dir direction with which to sort
    * @return new query which will sort the result by the field specified
    */
  def sortBy(field: Projection, dir: SortDirection): Repr

  /** method to add the intended projections. Intended to be called immediately before mapIterator by the project macro
    *
    * @param projs the projections to add
    * @return the query with the applied projection.
    */

  protected def addProjections(projs: List[Projection]): Repr

  def runQuery: Iterator[E]

  /** Sets the limit on how many results to return
    *
    * @param size maximum elements requested
    * @return new query with limited return size
    */
  def limit(size: Int): Repr

  def projectAndMap[T](projs: List[Projection], f: (DS, ObjectReader) => T): QueryIterator[T, E] = {
    val it = addProjections(projs).runQuery
    new QueryIterator[T, E]{
      def hasNext: Boolean = it.hasNext
      val ds: DS = self.ds
      val deserializer: (DS, ObjectReader) => T = f
      def nextEntity(): E = it.next()
    }
  }

  def getIterator(implicit deserializer: EntityBuilder[U, E]): QueryIterator[U with EntityBacker[U, E], E] = QueryIterator(ds, runQuery){ (ds, reader) =>
    deserializer.deserialize(ds, reader)
  }

  def mapIterator[T](f: (DS, ObjectReader) => T): QueryIterator[T, E] = projectAndMap(Nil, f)

  def project[R](f: U => R): QueryIterator[R, E] =     macro QueryMacros.project[U, R, E]

  def sortAsc(f: U => Any): Repr =                     macro QueryMacros.sortImplAsc[U, Repr]

  def sortDec(f: U => Any): Repr =                     macro QueryMacros.sortImplDesc[U, Repr]

  def filter(f: U => Boolean): Repr  =                 macro QueryMacros.filterImpl[U, Repr]
}

