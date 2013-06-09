package scalads
package core


import scalads.readers.ObjectReader

/**
 * @author Bryce Anderson
 *         Created on 6/1/13
 */

trait QueryIterator[+A, E]
     extends Iterator[A] { self =>

  def ds: AbstractDatastore[E]

  val deserializer: (AbstractDatastore[E], ObjectReader) => A

  def nextEntity(): E

  private[scalads] def newReader(entity: E): ObjectReader

  def nextWithEntity(): (E, A) = {
    val entity = nextEntity()
    (entity, deserializer(ds, newReader(entity)))
  }

  override def map[Z](f: (A) => Z): QueryIterator[Z,E] = new QueryIterator[Z,E] {
    def hasNext = self.hasNext

    def next() = f(self.next())

    def ds = self.ds

    val deserializer: (AbstractDatastore[E], ObjectReader) => Z = ((ds, r) => f(self.deserializer(ds, r)))

    def nextEntity() = self.nextEntity()

    private[scalads] def newReader(entity: E) = self.newReader(entity)
  }
}
