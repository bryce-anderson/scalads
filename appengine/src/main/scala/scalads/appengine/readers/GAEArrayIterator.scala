package scalads.appengine.readers

import java.util.Date
import com.google.appengine.api.datastore.PropertyContainer
import java.util.{Iterator => JIterator}
import scalads.readers.{ObjectReader, ArrayIterator}

/**
 * @author Bryce Anderson
 *         Created on 6/7/13
 */
class GAEArrayIterator(entity: JIterator[Any]) extends ArrayIterator { self =>

  // Option forms
  def nextObjectReader: ObjectReader = entity.next() match {
    case p: PropertyContainer => new GAEObjectReader(p, "")
    case p => failStructure(s"Type ${p.getClass} isn't Object type")
  }

  def nextArrayReader: ArrayIterator = entity.next() match {
    case c: java.util.Collection[Any] => new GAEArrayIterator(c.iterator())
    case p => failStructure(s"Type ${p.getClass} isn't Iterable type")
  }

  def nextInt: Int = entity.next() match {
    case i: Integer => i.intValue()
    case i: java.lang.Long => i.intValue()
    case p => failStructure(s"Type ${p.getClass} isn't Int type")
  }

  def nextLong: Long = entity.next() match {
    case i: Integer => i.longValue()
    case i: java.lang.Long => i.longValue()
    case p => failStructure(s"Type ${p.getClass} isn't Long type")
  }

  def nextFloat: Float = entity.next() match {
    case i: java.lang.Double => i.floatValue()
    case i: java.lang.Float => i.floatValue()
    case p => failStructure(s"Type ${p.getClass} isn't Float type")
  }

  def nextDouble: Double = entity.next() match {
    case i: java.lang.Double => i.doubleValue()
    case i: java.lang.Float => i.doubleValue()
    case p => failStructure(s"Type ${p.getClass} isn't Double type")
  }

  def nextBigInt: BigInt = BigInt(nextLong)

  def nextBigDecimal: BigDecimal = BigDecimal(nextDouble)

  def nextBool: Boolean = entity.next() match {
    case i: java.lang.Boolean => i.booleanValue()
    case p => failStructure(s"Type ${p.getClass} isn't Boolean type")
  }

  def nextString: String = entity.next() match {
    case s: String => s
    case p => failStructure(s"Type ${p.getClass} isn't String type")
  }

  def nextDate: Date = entity.next() match {
    case i: Date => i
    case p => failStructure(s"Type ${p.getClass} isn't Date type")
  }

  def hasNext: Boolean = entity.hasNext
}
