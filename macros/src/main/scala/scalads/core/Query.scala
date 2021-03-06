package scalads
package core

import language.experimental.macros

import scalads.macroimpls.QueryMacros
import scalads.Datastore
import scalads.readers.ObjectReader
import scala.collection.mutable.ListBuffer

/**
 * @author Bryce Anderson
 *         Created on 5/30/13
 */

trait Query[U, E] { self =>

  type Repr <: Query[U, E]

  type DS = Datastore[_, E]

  def ds: DS

  protected def transformer: Transformer[U, E]

  def remove(id: String): Unit

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
//
//  /** method to add the intended projections. Intended to be called immediately before mapIterator by the project macro
//    *
//    * @param projs the projections to add
//    * @return the query with the applied projection.
//    */
//  protected def addProjections(projs: List[Projection]): Repr
//
  def runQuery: Iterator[E]

  /** Sets the limit on how many results to return
    *
    * @param size maximum elements requested
    * @return new query with limited return size
    */
  def limit(size: Int): Repr

  def projectAndMap[T](projs: List[Projection], f: (DS, ObjectReader) => T): QueryIterator[T, E]

  def getIterator(): QueryIterator[U with EntityBacker[U, E], E] = QueryIterator(ds, runQuery, transformer)

  def mapIterator[T](f: (DS, ObjectReader) => T): QueryIterator[T, E] = projectAndMap(Nil, f)

  /** Update the results of the query with the supplied function
    *
    * @param f function applied to the returned entities to either updateOption or ignore the entry
    */
  def updateOption(f: U => Option[U]) {
    val newEntities = new ListBuffer[E]
    getIterator().foreach { i =>
      f(i).foreach{ r =>
        val newEntity = ds.replacementEntity(i.ds_entity)
        i.ds_serialize(r, transformer.newWriter(newEntity))
        newEntities += newEntity
      }
    }
    ds.putManyEntity(newEntities.result(): Iterable[E])
  }

  /** Update the results of the query with the supplied function
    *
    * @param f function applied to the returned entities to either updateOption or ignore the entry
    */
  def update(f: PartialFunction[U, U]): Unit = updateOption(f.lift)

  def project[R](f: U => R): QueryIterator[R, E] =     macro QueryMacros.project[U, R, E]

  def sortAsc(f: U => Any): Repr =                     macro QueryMacros.sortImplAsc[U, Repr]

  def sortDec(f: U => Any): Repr =                     macro QueryMacros.sortImplDesc[U, Repr]

  def filter(f: U => Boolean): Repr  =                 macro QueryMacros.filterImpl[U, Repr]
}

