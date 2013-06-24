package scalads.mongodb.readers

/**
 * @author Bryce Anderson
 *         Created on 6/15/13
 */

import reactivemongo.bson._

import scalads.readers.{ArrayIterator, ObjectReader}
import java.util.Date
import scala.util.Success

/**
 * @author Bryce Anderson
 *         Created on 4/29/13
 */


class BsonObjectReader(obj:BSONDocument) extends ObjectReader {

  def optDate(key: String): Option[Date] = obj.get(key) match {
    case t: BSONDateTime => Some(new Date(t.value))
    case _ => None
  }

  def optBytes(key: String): Option[Array[Byte]] = obj.get(key).flatMap(_ match {
    case bytes: BSONBinary => Some(bytes.value.readArray(bytes.value.size))
    case _ => None
  })

  lazy val getKeys: Set[String] = {
    ???
  }

  // Option forms
  def optObjectReader(key: String): Option[ObjectReader] = obj.get(key).flatMap(_ match {
    case obj: BSONDocument => Some(new BsonObjectReader(obj))
    case _ => None
  })

  def optArrayReader(key: String): Option[ArrayIterator] = obj.get(key).flatMap(_ match {
    case obj: BSONArray => Some(new BsonIterator(obj))
    case _ => None
  })

  def optInt(key: String): Option[Int] = optLong(key).map(_.asInstanceOf[Int])

  def optLong(key: String): Option[Long] = obj.get(key).flatMap(_ match {
    case BSONInteger(i) => Some(i)
    case _ => None
  })

  def optFloat(key: String): Option[Float] = optDouble(key).map(_.asInstanceOf[Float])

  def optDouble(key: String): Option[Double] = obj.get(key).flatMap(_ match {
    case BSONDouble(i) => Some(i)
    case _ => None
  })

  def optBigInt(key: String): Option[BigInt] = optLong(key).map(BigInt(_))

  def optBigDecimal(key: String): Option[BigDecimal] = optDouble(key).map(BigDecimal(_))

  def optBool(key: String): Option[Boolean] = obj.get(key).flatMap(_ match {
    case BSONBoolean(b) => Some(b)
    case _ => None
  })

  def optString(key: String): Option[String] = obj.get(key).flatMap(_ match {
    case BSONString(s) => Some(s)
    case _ => None
  })
}

class BsonIterator(entity: BSONArray) extends ArrayIterator {

  def nextDate: Date = next match {
    case obj: Date => obj
    case p => failStructure(s"Type ${p.getClass} isn't Date type")
  }

  val it = entity.iterator.map{
    case Success((_, v)) => v
    case e => this.failStructure(s"Iterator doesnt have next: $e")
  }

  private def next = if (it.hasNext) it.next() else null

  def nextObjectReader: BsonObjectReader = next match {
    case obj: BSONDocument => new BsonObjectReader(obj)
    case p => failStructure(s"Type ${p.getClass} isn't BsonObject type")
  }

  def nextArrayReader: BsonIterator = next match {
    case it: BSONArray => new BsonIterator(it)
    case p => failStructure(s"Type ${p.getClass} isn't BsonIterator type")
  }

  def nextInt: Int = next match {
    case BSONInteger(i) => i
    case p => failStructure(s"Type ${p.getClass} isn't Int type")
  }

  def nextLong: Long = nextInt.asInstanceOf[Long]

  def nextFloat: Float = nextDouble.asInstanceOf[Float]

  def nextDouble: Double = next match {
    case BSONDouble(d) => d
    case p => failStructure(s"Type ${p.getClass} isn't Double type")
  }

  def nextBigInt: BigInt = BigInt(nextLong)

  def nextBigDecimal: BigDecimal = BigDecimal(nextDouble)

  def nextBool: Boolean = next match {
    case BSONBoolean(b) => b
    case p => failStructure(s"Type ${p.getClass} isn't Boolean type")
  }

  def nextString: String = next match {
    case BSONString(s) => s
    case null => failStructure(s"Type null isn't String type")
  }

  def hasNext: Boolean = it.hasNext
}
