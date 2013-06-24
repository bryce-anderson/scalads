package scalads.mongodb.writers


import scalads.writers.Writer
import java.util.Date

import scalads.mongodb.{MongoDatastore, ScalaDSObject}
import reactivemongo.bson._
import scala.collection.mutable.ListBuffer

/**
 * @author Bryce Anderson
 *         Created on 6/15/13
 */

class MongoWriter(entity: ScalaDSObject) extends Writer[ScalaDSObject] {

  val root = new RootWriter(entity.json)

  private var writer: DSWriter = root

  def result = new ScalaDSObject(entity.collection, root.rootEntity)

  def startArray() { writer = writer.startArray() }

  def endArray() { writer = writer.endArray() }

  def startObject() {  writer = writer.startObject() }

  def endObject() {  writer = writer.endObject() }

  def startField(name: String) {  writer = writer.startField(name) }

  def int(in: Int) { writer = writer.handleVal(in) }

  def string(in: String) { writer = writer.handleVal(in) }

  def float(in: Float) { writer = writer.handleVal(in) }

  def double(in: Double) { writer = writer.handleVal(in) }

  def bigDecimal(in: BigDecimal) { writer = writer.handleVal(in) }

  def bigInt(in: BigInt) { writer = writer.handleVal(in) }

  def short(in: Short) { writer = writer.handleVal(in) }

  def bytes(in: Array[Byte]) { writer = writer.handleVal(in) }

  def long(in: Long) { writer = writer.handleVal(in) }

  def boolean(in: Boolean) { writer = writer.handleVal(in) }

  def date(in: Date) { writer = writer.handleVal(in) }

  def nil() { writer = writer.handleVal(null) }
}

trait DSWriter { self =>

  def startObject(): ObjectWriter

  def startArray(): ArrayWriter

  private[writers] def handleVal(value: Any): DSWriter

  protected final def error(op: String) = sys.error(s"Writer ${this.getClass.toString} cannot perform $op")

  def endObject(): DSWriter

  def endArray(): DSWriter

  def startField(name: String): DSWriter = error("startField")
}

class RootWriter(var rootEntity: BSONDocument) extends DSWriter { self =>

  def startArray(): ArrayWriter = error("startArray")

  def handleVal(value: Any): DSWriter = error("handleVal")

  def endArray(): DSWriter = error("endArray")

  private var finished = false

  override def startObject(): ObjectWriter = {
    if (finished) sys.error("RootWriter already started. Cannot start a new object!")
    else new ObjectWriter({doc => rootEntity = doc; self}, Nil)
  }

  override def endObject() = { finished = true; self }
}

// On ending this object, the parent is returned.
private[writers] class ObjectWriter(finish: BSONDocument => DSWriter, fields: List[(String, BSONValue)]) extends DSWriter { self =>

  def endObject(): DSWriter = finish(BSONDocument(fields))

  def endArray(): DSWriter = error("endArray")

  override def handleVal(value: Any): DSWriter = error("handleVal")

  def startObject(): ObjectWriter = error("startObject")

  def startArray() = error("startArray")

  override def startField(name: String) = new FieldWriter(name, v => new ObjectWriter(finish, (name, v)::fields))
}

private[writers] class FieldWriter(name: String, finish: BSONValue => DSWriter) extends DSWriter with MongoHandleWrapper { self =>

  def endObject(): DSWriter = error("endObject")

  def endArray(): DSWriter = error("endArray")

  override def startObject(): ObjectWriter = {
    new ObjectWriter(finish, Nil)
  }

  override def startArray(): ArrayWriter = new ArrayWriter(finish, new ListBuffer[BSONValue])

  override def handleVal(value: Any): DSWriter =  finish(mongoHandle(value))
}

// on ending the array, the array is added using parent.handleVal and the reader returned should be the underlying reader
private[writers] class ArrayWriter(finish: BSONArray => DSWriter, array: ListBuffer[BSONValue]) extends DSWriter with MongoHandleWrapper { self =>

  def endObject(): DSWriter = error("endObject")

  def endArray(): DSWriter = finish(BSONArray(array.result))

  def startObject(): ObjectWriter = {
    new ObjectWriter({obj =>
      array += obj
      self}, Nil)
  }

  def startArray(): ArrayWriter = new ArrayWriter({ arr =>
    array += arr
    self
  }, new ListBuffer[BSONValue])

  override def handleVal(value: Any): DSWriter = {
    array += mongoHandle(value)
    self
  }
}

// Trait that holds a function that will filter types as necessary
trait MongoHandleWrapper {
  def mongoHandle(in: Any): BSONValue = MongoDatastore.mongoHandle(in)
}
