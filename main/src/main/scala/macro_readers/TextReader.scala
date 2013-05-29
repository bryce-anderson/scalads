package macro_readers

import annotation.tailrec
import collection.mutable.ListBuffer
import runtime.ScalaRunTime
import java.util.Date
import com.google.appengine.repackaged.org.joda.time.format.DateTimeFormat
import java.text.SimpleDateFormat

/**
 * @author Bryce Anderson
 * Created on 3/25/13 at 8:37 PM
 */

final class JsonTextObjectReader(cursor: JsonTextCursor) extends ObjectReader {

  override def equals(other: Any) = other match {
    case other: JsonTextObjectReader => this.fields == other.fields
    case _ => false
  }


  override def hashCode(): Int = ScalaRunTime._hashCode(Tuple1(fields))

  val fields: List[(String, JsonField)] = {
    if(cursor.nextChar() != '{' ) failStructure(s"Json is not an object")

    val builder = new collection.mutable.ListBuffer[(String, JsonField)]()

    @tailrec
    def looper(): Unit = {
      val JsonString(key) = cursor.findNextString()
      if (cursor.zoomPastSeparator(':', '}')) cursor.failParse(s"Invalid json.")
      val value = cursor.extractField()
      builder += ((key, value))
      if(!cursor.zoomPastSeparator(',', '}')) {
        looper()
      }
    }
    looper()
    if(cursor.nextChar() != '}' ) cursor.failParse(s"Json object missing closing bracket.")
    builder.result
  }

  def getField(name: String) = {
    @tailrec def looper(lst: List[(String, JsonField)]): Option[JsonField] = lst match {
      case Nil => None
      case l if (l.head._1 == name) => Some(l.head._2)
      case l => looper(l.tail)
    }
    looper(fields)
  }

  lazy val getKeys: Set[String] = fields.map(_._1).toSet

  // Option forms
  def optObjectReader(key: String): Option[ObjectReader] = getField(key).map ( _ match {
    case JsonObject(reader) => reader
    case e => failStructure(s"Field '$key' doesn't contain object: ${e.toString}")
  })

  def optArrayReader(key: String): Option[ArrayIterator] = getField(key).map ( _ match {
    case JsonArray(reader) => reader
    case e => failStructure(s"Field '$key' doesn't contain array: ${e.toString}")
  })

  def optInt(key: String): Option[Int] = getField(key).map ( _ match {
    case n: JsonNumber => n.toInt
    case e => failStructure(s"Field '$key' doesn't contain number: ${e.toString}")
  })

  def optLong(key: String): Option[Long] = getField(key).map ( _ match {
    case n: JsonNumber => n.toLong
    case e => failStructure(s"Field '$key' doesn't contain number: ${e.toString}")
  })

  def optFloat(key: String): Option[Float] = getField(key).map ( _ match {
    case n: JsonNumber => n.toFloat
    case e => failStructure(s"Field '$key' doesn't contain number: ${e.toString}")
  })

  def optDouble(key: String): Option[Double] = getField(key).map ( _ match {
    case n: JsonNumber => n.toDouble
    case e => failStructure(s"Field '$key' doesn't contain number: ${e.toString}")
  })

  def optBigInt(key: String): Option[BigInt] = getField(key).map ( _ match {
    case n: JsonNumber => n.toBigInt
    case e => failStructure(s"Field '$key' doesn't contain number: ${e.toString}")
  })

  def optBigDecimal(key: String): Option[BigDecimal] = getField(key).map ( _ match {
    case n: JsonNumber => n.toBigDecimal
    case e => failStructure(s"Field '$key' doesn't contain number: ${e.toString}")
  })

  def optBool(key: String): Option[Boolean] = getField(key).map ( _ match {
    case JsonBool(v) => v
    case e => failStructure(s"Field '$key' doesn't contain Boolean: ${e.toString}")
  })

  def optString(key: String): Option[String] = getField(key).map ( _ match {
    case JsonString(str) => str
    case e => failStructure(s"Field '$key' doesn't contain number: ${e.toString}")
  })

  def optDate(key: String): Option[Date] = optString(key).map(new SimpleDateFormat().parse(_))
}

final class TextArrayIterator(cursor: JsonTextCursor) extends ArrayIterator {

  override def equals(other: Any) = other match {
    case other: TextArrayIterator => this.fields == other.fields
    case _ => false
  }


  // TODO: THIS IS BAD, a hashcode should not use a mutable variable
  override def hashCode(): Int = ScalaRunTime._hashCode(Tuple1(fields))

  var fields: List[JsonField] = {
    if(cursor.nextChar() != '[' ) cursor.failParse(s"Json is not an array.")
    val builder = new ListBuffer[JsonField]
    @tailrec
    def looper(): Unit = {
      builder += cursor.extractField()
      if(!cursor.zoomPastSeparator(',', ']')) looper()
    }

    looper()
    if(cursor.nextChar() != ']' ) cursor.failParse(s"Json object missing closing bracket.")
    builder.result()
  }

  private def nextField = {
    if(fields.isEmpty) failStructure(s"TextArrayIterator is empty!")
    val value = fields.head
    fields = fields.tail
    value
  }

  def hasNext: Boolean = !fields.isEmpty

  // Option forms
  def getNextObjectReader: Option[ObjectReader] = nextField match {
    case JsonObject(obj) => Some(obj)
    case _ => None
  }

  def getNextArrayReader: Option[ArrayIterator] = nextField match {
    case JsonArray(arr) => Some(arr)
    case _ => None
  }

  def getNextInt: Option[Int] = nextField match {
    case n: JsonNumber => Some(n.toInt)
    case _ => None
  }

  def getNextLong: Option[Long] = nextField match {
    case n: JsonNumber => Some(n.toLong)
    case _ => None
  }

  def getNextFloat: Option[Float] = nextField match {
    case n: JsonNumber => Some(n.toFloat)
    case _ => None
  }

  def getNextDouble: Option[Double] = nextField match {
    case n: JsonNumber => Some(n.toDouble)
    case _ => None
  }

  def getNextBigInt: Option[BigInt] = nextField match {
    case n: JsonNumber => Some(n.toBigInt)
    case _ => None
  }

  def getNextBigDecimal: Option[BigDecimal] = nextField match {
    case n: JsonNumber => Some(n.toBigDecimal)
    case _ => None
  }

  def getNextBool: Option[Boolean] = nextField match {
    case JsonBool(v) => Some(v)
    case _ => None
  }

  def getNextString: Option[String] = nextField match {
    case JsonString(str) => Some(str)
    case _ => None
  }

  def getNextDate: Option[Date] = getNextString.map(new SimpleDateFormat().parse(_))
}
