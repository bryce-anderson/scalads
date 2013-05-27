package macro_readers

import scala.annotation.tailrec

/**
 * @author Bryce Anderson
 * Created on 3/28/13 at 8:28 PM
 */

object JsonTextReader {
  def bindText(str: String): JsonReader = {
    val cursor = new JsonStringCursor(str)
    cursor.extractField() match {
      case JsonObject(obj) => obj
      case JsonArray(obj)  => obj
      case e => throw new java.lang.IllegalStateException(s"Invalid starting json structure: $e")
    }
  }
}

sealed trait JsonField
case object Null extends JsonField
case class JsonBool(v: Boolean) extends JsonField
case class JsonString(str: String) extends JsonField
case class JsonObject(reader: JsonObjectReader) extends JsonField
case class JsonArray(reader: JsonArrayIterator) extends JsonField
case class JsonNumber(str: String) extends JsonField {
  def toInt = str.toInt
  def toLong = str.toLong
  def toBigInt = BigInt(str)
  def toDouble = str.toDouble
  def toFloat = str.toFloat
  def toBigDecimal = BigDecimal(str)
}

trait JsonTextCursor {
  def empty: Boolean
  def findNextString(): JsonString

  // Will move past separator, trimming until next char. If at end, leaves end char.
  def zoomPastSeparator(sep: Char, end: Char): Boolean
  def findNextNumber(): JsonNumber
  def extractField(): JsonField
  def findNextBoolean(): JsonBool
  def nextChar(): Char

  protected def isNumberChar(c: Char) = (Character.isDigit(c) || c == '.' || c == 'e' || c == 'E' || c == '-' || c == '+')
  protected def isWhitespace(in: Char) = ( in == ' ' || in == '\r' || in == '\t' || in == '\n')

  def failStructure(msg: String) = sys.error(msg)
  def failParse(msg: String) = sys.error(msg)
}


