package scalads.appengine.readers

import java.util.Date
import com.google.appengine.api.datastore.{Entity, RawValue, PropertyContainer, Text, Blob, ShortBlob}


import scala.collection.JavaConverters._

import scalads.readers.{ArrayIterator, ObjectReader}
import scala.Some

/**
 * @author Bryce Anderson
 *         Created on 5/29/13
 */

object GAEObjectReader {
  def apply(entity: Entity) = new GAEObjectReader(entity, "")
}

class GAEObjectReader(entity: PropertyContainer, prefix: String) extends ObjectReader { self =>

  lazy val getKeys: Set[String] = entity.getProperties.keySet().asScala.toSet

  private val fullPrefix = if(prefix.isEmpty) "" else prefix + "."

  // Option forms
  def optObjectReader(key: String): Option[GAEObjectReader] = Some(new GAEObjectReader(entity, fullPrefix + key))

  def optArrayReader(key: String): Option[ArrayIterator] = Option(entity.getProperty(fullPrefix + key)).flatMap(_ match {
    case lst: java.util.List[Any] => Some(new GAEArrayIterator(lst.iterator()))
    case r: RawValue => try {
      println(s"Found raw:" + r)
      Some(new GAEArrayIterator(r.asStrictType(classOf[java.util.List[Any]]).iterator()))
    } catch { case _: Throwable => None }
    case e => None
  })

  def optInt(key: String): Option[Int] = Option(entity.getProperty(fullPrefix + key)).flatMap (_ match {
    case i: java.lang.Integer =>  Some(i.intValue())
    case i: java.lang.Long => Some(i.intValue())
    case r: RawValue => try { Some(r.asStrictType(classOf[java.lang.Long]).intValue()) } catch { case _: Throwable => None }
    case _ => None
  })

  def optLong(key: String): Option[Long] = Option(entity.getProperty(fullPrefix + key)).flatMap (_ match {
    case i: Integer =>  Some(i.longValue())
    case i: java.lang.Long => Some(i.longValue())
    case r: RawValue => try { Some(r.asStrictType(classOf[java.lang.Long]).longValue()) } catch { case _: Throwable => None }
    case _ => None
  })

  def optFloat(key: String): Option[Float] = Option(entity.getProperty(fullPrefix + key)).flatMap (_ match {
    case i: java.lang.Double =>   Some(i.floatValue())
    case i: java.lang.Float => Some(i.floatValue())
    case r: RawValue => try { Some(r.asStrictType(classOf[java.lang.Double]).floatValue()) } catch { case _: Throwable => None }
    case _ => None
  })

  def optDouble(key: String): Option[Double] = Option(entity.getProperty(fullPrefix + key)).flatMap (_ match {
    case i: java.lang.Double =>   Some(i.doubleValue())
    case i: java.lang.Float => Some(i.doubleValue())
    case r: RawValue => try { Some(r.asStrictType(classOf[java.lang.Double]).doubleValue()) } catch { case _: Throwable => None }
    case _ => None
  })

  def optBigInt(key: String): Option[BigInt] = optLong(key).map(BigInt(_))

  def optBigDecimal(key: String): Option[BigDecimal] = optDouble(key).map(BigDecimal(_))

  def optBool(key: String): Option[Boolean] =  Option(entity.getProperty(fullPrefix + key)).flatMap (_ match {
    case i: java.lang.Boolean => Some(i.booleanValue())
    case r: RawValue => try { Some(r.asStrictType(classOf[java.lang.Boolean]).booleanValue()) } catch { case _: Throwable => None }
    case _ => None
  })

  def optString(key: String): Option[String] =  Option(entity.getProperty(fullPrefix + key)).flatMap (_ match {
    case i: String => Some(i)
    case s: Text =>   Some(s.getValue)
    case r: RawValue => try { Some(r.asStrictType(classOf[String])) } catch { case _: Throwable => None }
    case e => None
  })

  def optDate(key: String): Option[Date] =  Option(entity.getProperty(fullPrefix + key)).flatMap (_ match {
    case i: Date => Some(i)
    case r: RawValue => try { Some(r.asStrictType(classOf[Date])) } catch { case _: Throwable => None }
    case _ => None
  })

  def optBytes(key: String): Option[Array[Byte]] = Option(entity.getProperty(fullPrefix + key)).flatMap(_ match {
    case i: ShortBlob => Some(i.getBytes)
    case i: Blob      => Some(i.getBytes)
      // TODO: need to check both type when pulling from a raw value
    case r: RawValue => try { Some(r.asStrictType(classOf[Blob]).getBytes) } catch { case _: Throwable => None }
    case _ => None

  })
}
