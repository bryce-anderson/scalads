package scalads.readers

import java.util.Date
import com.google.appengine.api.datastore.{Entity, Text}

import scala.collection.JavaConverters._

/**
 * @author Bryce Anderson
 *         Created on 5/29/13
 */

object GAEObjectReader {
  def apply(entity: Entity) = new GAEObjectReader(entity, "")
}

class GAEObjectReader(val entity: Entity, prefix: String) extends ObjectReader { self =>

  lazy val getKeys: Set[String] = entity.getProperties.keySet().asScala.toSet

  private val fullPrefix = if(prefix.isEmpty) "" else prefix + "."

  // Option forms
  def optObjectReader(key: String): Option[GAEObjectReader] =
    Some(new GAEObjectReader(entity, fullPrefix + key))

  override def getObjectReader(key: String): GAEObjectReader =
    optObjectReader(key).getOrElse(failStructure(s"Cannot find reader with key $key", self))

  def optArrayReader(key: String): Option[ArrayIterator] = failStructure("GAE cant access sequences", self)

  def optInt(key: String): Option[Int] = Option(entity.getProperty(fullPrefix + key)).flatMap (_ match {
    case i: java.lang.Integer =>  Some(i.intValue())
    case i: java.lang.Long => Some(i.intValue())
    case _ => None
  })

  def optLong(key: String): Option[Long] = Option(entity.getProperty(fullPrefix + key)).flatMap (_ match {
    case i: Integer =>  Some(i.longValue())
    case i: java.lang.Long => Some(i.longValue())
    case _ => None
  })

  def optFloat(key: String): Option[Float] = Option(entity.getProperty(fullPrefix + key)).flatMap (_ match {
    case i: java.lang.Double =>   Some(i.floatValue())
    case i: java.lang.Float => Some(i.floatValue())
    case _ => None
  })

  def optDouble(key: String): Option[Double] = Option(entity.getProperty(fullPrefix + key)).flatMap (_ match {
    case i: java.lang.Double =>   Some(i.doubleValue())
    case i: java.lang.Float => Some(i.doubleValue())
    case _ => None
  })

  def optBigInt(key: String): Option[BigInt] = optLong(key).map(BigInt(_))

  def optBigDecimal(key: String): Option[BigDecimal] = optDouble(key).map(BigDecimal(_))

  def optBool(key: String): Option[Boolean] =  Option(entity.getProperty(fullPrefix + key)).flatMap (_ match {
    case i: java.lang.Boolean => Some(i.booleanValue())
    case _ => None
  })

  def optString(key: String): Option[String] =  Option(entity.getProperty(fullPrefix + key)).flatMap (_ match {
    case i: String => Some(i)
    case s: Text =>   Some(s.getValue)
    case _ => None
  })

  def optDate(key: String): Option[Date] =  Option(entity.getProperty(fullPrefix + key)).flatMap (_ match {
    case i: Date => Some(i)
    case _ => None
  })
}
