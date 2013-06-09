package scalads.readers

import java.util.Date

/**
 * @author Bryce Anderson
 * Created on 3/23/13 at 12:39 PM
 */

sealed trait Reader {
  def failStructure(msg: String, thrower: Reader = null) = {
    val fullMessage = if(thrower != null) (msg.concat(" Thrower: " + thrower.getClass.toString())) else msg
    sys.error(fullMessage)
  }
}

trait ObjectReader extends Reader {
  type E <: AnyRef

  def entity: E

  def getKeys: Set[String]

  def hasKey(str: String) = getKeys.exists( _ == str)

  // Option forms
  def optObjectReader(key: String): Option[ObjectReader]
  def optArrayReader(key: String): Option[ArrayIterator]
  def optInt(key: String): Option[Int]
  def optLong(key: String): Option[Long]
  def optFloat(key: String): Option[Float]
  def optDouble(key: String): Option[Double]
  def optBigInt(key: String): Option[BigInt]
  def optBigDecimal(key: String): Option[BigDecimal]
  def optBool(key: String): Option[Boolean]
  def optString(key: String): Option[String]
  def optDate(key: String): Option[Date]
  def optBytes(key: String): Option[Array[Byte]]

  // Direct forms with default impl based on the Option form
  def getObjectReader(key: String): ObjectReader =
    optObjectReader(key).getOrElse(failStructure(s"ObjectReader doesn't contain an object in field '$key'"))
  def getArrayReader(key: String): ArrayIterator =
    optArrayReader(key).getOrElse(failStructure(s"ObjectReader doesn't contain an array in field '$key'"))
  def getInt(key: String): Int =
    optInt(key).getOrElse(failStructure(s"ObjectReader doesn't contain an Int in field '$key'"))
  def getLong(key: String): Long =
    optLong(key).getOrElse(failStructure(s"ObjectReader doesn't contain an Long in field '$key'"))
  def getFloat(key: String): Float =
    optFloat(key).getOrElse(failStructure(s"ObjectReader doesn't contain a Float in field '$key'"))
  def getDouble(key: String): Double =
    optDouble(key).getOrElse(failStructure(s"ObjectReader doesn't contain a Double in field '$key'"))
  def getBigInt(key: String): BigInt =
    optBigInt(key).getOrElse(failStructure(s"ObjectReader doesn't contain a BigInt in field '$key'"))
  def getBigDecimal(key: String): BigDecimal =
    optBigDecimal(key).getOrElse(failStructure(s"ObjectReader doesn't contain a BigDecimal in field '$key'"))
  def getBool(key: String): Boolean =
    optBool(key).getOrElse(failStructure(s"ObjectReader doesn't contain a Boolean in field '$key'"))
  def getString(key: String): String =
    optString(key).getOrElse(failStructure(s"ObjectReader doesn't contain a String in field '$key'"))
  def getDate(key: String): Date =
    optDate(key).getOrElse(failStructure(s"ObjectReader doesn't contain a Date in field '$key'"))
  def getBytes(key: String): Array[Byte] =
    optBytes(key).getOrElse(failStructure(s"ObjectReader doesn't contain bytes in field '$key'"))
}

trait ArrayIterator extends Reader {
  type E <: AnyRef
  def entity: E

  // Direct forms with default impl based on the option forms
  def hasNext: Boolean
  def nextObjectReader: ObjectReader
  def nextArrayReader: ArrayIterator
  def nextInt: Int
  def nextLong: Long
  def nextFloat: Float
  def nextDouble: Double
  def nextBigInt: BigInt
  def nextBigDecimal: BigDecimal
  def nextBool: Boolean
  def nextString: String
  def nextDate: Date
}
