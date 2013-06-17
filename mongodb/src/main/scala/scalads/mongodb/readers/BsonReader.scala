package scalads.mongodb.readers

/**
 * @author Bryce Anderson
 *         Created on 6/15/13
 */

import org.bson.{BasicBSONObject, BSONObject}
import org.bson.types.{Binary, BasicBSONList}
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

class BsonObjectReader(obj:BSONObject) extends ObjectReader {


  type Entity = BSONObject

  def entity = obj

  def optDate(key: String): Option[Date] = obj.get(key) match {
    case date: Date => Some(date)
    case _ => None
  }

  def optBytes(key: String): Option[Array[Byte]] = obj.get(key) match {
    case bytes: Binary => Some(bytes.getData)
    case _ => None
  }

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
    case s: String => Some(BigInt(s))
    case _ => None
  }

  def optBigDecimal(key: String): Option[BigDecimal] = obj.get(key) match {
    case i: java.lang.Float => Some(BigDecimal(i.toDouble))
    case i: java.lang.Double => Some(BigDecimal(i))
    case s: String => Some(BigDecimal(s))
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

class BsonIterator(val entity: BasicBSONList) extends ArrayIterator {


  type Entity = BasicBSONList

  def nextDate: Date = next match {
    case obj: Date => obj
    case p => failStructure(s"Type ${p.getClass} isn't Date type")
  }

  private val it = entity.iterator()

  private def next = if (it.hasNext) it.next() else null

  def nextObjectReader: BsonObjectReader = next match {
    case obj: BasicBSONObject => new BsonObjectReader(obj)
    case p => failStructure(s"Type ${p.getClass} isn't BsonObject type")
  }

  def nextArrayReader: BsonIterator = next match {
    case it: BasicBSONList => new BsonIterator(it)
    case p => failStructure(s"Type ${p.getClass} isn't BsonIterator type")
  }

  def nextInt: Int = next match {
    case i: java.lang.Integer => i.toInt
    case i: java.lang.Long => i.toInt
    case p => failStructure(s"Type ${p.getClass} isn't Int type")
  }

  def nextLong: Long = next match {
    case i: java.lang.Integer => i.toLong
    case i: java.lang.Long => i.longValue()
    case p => failStructure(s"Type ${p.getClass} isn't Long type")
  }

  def nextFloat: Float = next match {
    case i: java.lang.Float => i.toFloat
    case i: java.lang.Double => i.toFloat
    case p => failStructure(s"Type ${p.getClass} isn't Float type")
  }

  def nextDouble: Double = next match {
    case i: java.lang.Float => i.toDouble
    case i: java.lang.Double => i.toDouble
    case p => failStructure(s"Type ${p.getClass} isn't Double type")
  }

  def nextBigInt: BigInt = next match {
    case i: java.lang.Integer => BigInt(i)
    case i: java.lang.Long => BigInt(i)
    case i: String => BigInt(i)
    case p => failStructure(s"Type ${p.getClass} isn't BigInt type")
  }

  def nextBigDecimal: BigDecimal = next match {
    case i: java.lang.Float => BigDecimal(i.toDouble)
    case i: java.lang.Double => BigDecimal(i)
    case i: String => BigDecimal(i)
    case p => failStructure(s"Type ${p.getClass} isn't BigDecimal type")
  }

  def nextBool: Boolean = next match {
    case b: java.lang.Boolean => b.booleanValue()
    case p => failStructure(s"Type ${p.getClass} isn't Boolean type")
  }

  def nextString: String = next match {
    case s: String => s
    case null => failStructure(s"Type null isn't String type")
  }

  def hasNext: Boolean = it.hasNext()
}
