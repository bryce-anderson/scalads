package scalads.mongodb.readers

/**
 * @author Bryce Anderson
 *         Created on 6/15/13
 */

import org.bson.{BasicBSONObject, BSONObject}
import org.bson.types.BasicBSONList
import scalads.readers.{ArrayIterator, ObjectReader}
import java.util.Date

/**
 * @author Bryce Anderson
 *         Created on 4/29/13
 */

object BsonReader {
  def apply(obj: BSONObject) = obj match {
    case obj: BasicBSONObject => new BsonObjectReader(obj)
    case lst: BasicBSONList => new BsonIterator(lst)
  }
}

class BsonObjectReader(obj: BasicBSONObject) extends ObjectReader {


  type Entity = BSONObject

  def entity = obj

  def optDate(key: String): Option[Date] =

  def optBytes(key: String): Option[Array[Byte]] =

  lazy val getKeys: Set[String] = {
    import scala.collection.JavaConversions._
    obj.keySet().toSet
  }

  // Option forms
  def optObjectReader(key: String): Option[ObjectReader] = obj.get(key) match {
    case obj: BasicBSONObject => Some(new BsonObjectReader(obj))
    case _ => None
  }

  def optArrayReader(key: String): Option[ArrayIterator] = obj.get(key) match {
    case obj: BasicBSONList => Some(new BsonIterator(obj))
    case _ => None
  }

  def optInt(key: String): Option[Int] = obj.get(key) match {
    case i: Integer => Some(i.toInt)
    case i: java.lang.Long => Some(i.toInt)
    case _ => None
  }

  def optLong(key: String): Option[Long] = obj.get(key) match {
    case i: Integer => Some(i.toLong)
    case i: java.lang.Long => Some(i.longValue())
    case _ => None
  }

  def optFloat(key: String): Option[Float] = obj.get(key) match {
    case i: java.lang.Float => Some(i.toFloat)
    case i: java.lang.Double => Some(i.toFloat)
    case _ => None
  }

  def optDouble(key: String): Option[Double] = obj.get(key) match {
    case i: java.lang.Float => Some(i.toDouble)
    case i: java.lang.Double => Some(i.toDouble)
    case _ => None
  }

  def optBigInt(key: String): Option[BigInt] = obj.get(key) match {
    case i: Integer => Some(BigInt(i))
    case i: java.lang.Long => Some(BigInt(i))
    case _ => None
  }

  def optBigDecimal(key: String): Option[BigDecimal] = obj.get(key) match {
    case i: java.lang.Float => Some(BigDecimal(i.toDouble))
    case i: java.lang.Double => Some(BigDecimal(i))
    case _ => None
  }

  def optBool(key: String): Option[Boolean] = obj.get(key) match {
    case b: java.lang.Boolean => Some(b.booleanValue())
    case _ => None
  }

  def optString(key: String): Option[String] = obj.get(key) match {
    case i: String => Some(i)
    case null => None
    case i => Some(i.toString)
  }
}

class BsonIterator(lst: BasicBSONList) extends ArrayIterator {

  private val it = lst.iterator()

  private def next = if (it.hasNext) it.next() else null

  def getNextObjectReader: Option[ObjectReader] = next match {
    case obj: BasicBSONObject => Some(new BsonObjectReader(obj))
    case _ => None
  }

  def getNextArrayReader: Option[ArrayIterator] = next match {
    case it: BasicBSONList => Some(new BsonIterator(it))
    case _ => None
  }

  def getNextInt: Option[Int] = next match {
    case i: Integer => Some(i.toInt)
    case i: java.lang.Long => Some(i.toInt)
    case _ => None
  }

  def getNextLong: Option[Long] = next match {
    case i: Integer => Some(i.toLong)
    case i: java.lang.Long => Some(i.longValue())
    case _ => None
  }

  def getNextFloat: Option[Float] = next match {
    case i: java.lang.Float => Some(i.toFloat)
    case i: java.lang.Double => Some(i.toFloat)
    case _ => None
  }

  def getNextDouble: Option[Double] = next match {
    case i: java.lang.Float => Some(i.toDouble)
    case i: java.lang.Double => Some(i.toDouble)
    case _ => None
  }

  def getNextBigInt: Option[BigInt] = next match {
    case i: Integer => Some(BigInt(i))
    case i: java.lang.Long => Some(BigInt(i))
    case _ => None
  }

  def getNextBigDecimal: Option[BigDecimal] = next match {
    case i: java.lang.Float => Some(BigDecimal(i.toDouble))
    case i: java.lang.Double => Some(BigDecimal(i))
    case _ => None
  }

  def getNextBool: Option[Boolean] = next match {
    case b: java.lang.Boolean => Some(b.booleanValue())
    case _ => None
  }

  def getNextString: Option[String] = next match {
    case s: String => Some(s)
    case null => None
    case e => Some(e.toString)
  }

  def hasNext: Boolean = it.hasNext()
}
