package scalads
package core


import scalads.readers.ObjectReader
import scalads.macroimpls.EntityBuilder

/**
 * @author Bryce Anderson
 *         Created on 6/1/13
 */

trait QueryIterator[+A, E]
     extends Iterator[A] { self =>

  type DS = AbstractDatastore[_, E]

  def ds: DS

  val deserializer: (DS, ObjectReader) => A

  def nextEntity(): E

  def next(): A = deserializer(ds, ds.newReader(nextEntity()))

  def nextWithEntity(): (E, A) = {
    val entity = nextEntity()
    (entity, deserializer(ds, ds.newReader(entity)))
  }

  override def map[Z](f: (A) => Z): QueryIterator[Z,E] = new QueryIterator[Z,E] {
    def hasNext = self.hasNext

    def ds = self.ds

    val deserializer: (DS, ObjectReader) => Z = ((ds, r) => f(self.deserializer(ds, r)))

    def nextEntity() = self.nextEntity()
  }
}

object QueryIterator {
  def apply[A, E](datastore: AbstractDatastore[_, E], it: Iterator[E])(f: (AbstractDatastore[_, E], ObjectReader) => A) =
    new QueryIterator[A, E] {
      def hasNext: Boolean = it.hasNext

      def ds = datastore

      val deserializer: (DS, ObjectReader) => A = f

      def nextEntity(): E = it.next()
    }
}
