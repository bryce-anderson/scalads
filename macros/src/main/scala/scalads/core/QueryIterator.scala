package scalads
package core


import scalads.readers.ObjectReader

/**
 * @author Bryce Anderson
 *         Created on 6/1/13
 */

trait QueryIterator[+A, E, DS <: AbstractDatastore { type Entity = E }]
     extends Iterator[A] { self =>


  def ds: DS

  val deserializer: (DS, ObjectReader) => A

  def nextEntity(): E

  def next(): A = deserializer(ds, ds.newReader(nextEntity()))

  def nextWithEntity(): (E, A) = {
    val entity = nextEntity()
    (entity, deserializer(ds, ds.newReader(entity)))
  }

  override def map[Z](f: (A) => Z): QueryIterator[Z,E,DS] = new QueryIterator[Z,E,DS] {
    def hasNext = self.hasNext

    def ds = self.ds

    val deserializer: (DS, ObjectReader) => Z = ((ds, r) => f(self.deserializer(ds, r)))

    def nextEntity() = self.nextEntity()
  }
}

object QueryIterator {
  def apply[A, E, DS <: AbstractDatastore{type Entity = E}](datastore: DS, it: Iterator[E], f: (DS, ObjectReader) => A) = new QueryIterator[A, E, DS] {
    def hasNext: Boolean = it.hasNext

    def ds: DS = datastore

    val deserializer: (DS, ObjectReader) => A = f

    def nextEntity(): E = it.next()
  }
}
