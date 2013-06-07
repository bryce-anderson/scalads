package scalads.writers

import java.util.Date

/**
 * @author Bryce Anderson
 *         Created on 5/25/13
 */

// Writers are stateful
trait Writer[T] {
  def result: T

  def startArray()
  def endArray()
  def startObject(objType: String)
  def endObject()
  def startField(name: String)

  def int(in: Int)
  def string(in: String)
  def float(in: Float)
  def double(in: Double)
  def bigDecimal(in: BigDecimal)
  def bigInt(in: BigInt)
  def short(in: Short)
  def bytes(in: Array[Byte])
  def long(in: Long)
  def boolean(in: Boolean)
  def date(in: Date)
  def nil()
}