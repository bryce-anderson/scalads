package scalads
package core


import scalads.readers.ObjectReader
import scalads.macroimpls.EntityBuilder

/**
 * @author Bryce Anderson
 *         Created on 6/1/13
 */

trait QueryIterator[+U, E]
     extends Iterator[U] { self =>

  def nextEntity(): E

  def next(): U

  override def map[Z](f: (U) => Z): QueryIterator[Z,E] = new QueryIterator[Z,E] {
    def hasNext = self.hasNext

    def nextEntity() = self.nextEntity()

    def next(): Z = f(self.next())
  }
}

object QueryIterator {
  def apply[A, E](datastore: AbstractDatastore[_, E], it: Iterator[E], transformer: Transformer[A, E]) =
    new QueryIterator[A with EntityBacker[A, E], E] {
      def hasNext: Boolean = it.hasNext

      def nextEntity(): E = it.next()

      def next(): A with EntityBacker[A, E] =
        transformer.deserializer.deserialize(datastore, transformer, nextEntity())
    }
}
