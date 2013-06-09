package scalads.appengine.writers

import language.experimental.macros

import java.util.Date
import com.google.appengine.api.datastore._
import java.util
import scalads.writers.Writer

/**
 * @author Bryce Anderson
 *         Created on 5/27/13
 */


class GAEWriter(entity: Entity) extends Writer[Entity] {

  private var writer: DSWriter = new RootWriter(entity)

  def result = entity

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
  protected def parent: DSWriter
  protected def container: PropertyContainer

  def startObject(): ObjectWriter
  def startArray(): ArrayWriter

  private[writers] def handleVal(value: Any): DSWriter

  protected final def error(op: String) = sys.error(s"Writer ${this.getClass.toString} cannot perform $op")

  def endObject(): DSWriter

  def endArray(): DSWriter

  def startField(name: String): DSWriter = error("startField")
}

class RootWriter(val rootEntity: Entity) extends DSWriter { self =>

  protected def container: PropertyContainer = rootEntity

  def startArray(): ArrayWriter = error("startArray")

  def handleVal(value: Any): DSWriter = error("handleVal")

  def endArray(): DSWriter = error("endArray")

  def parent: DSWriter = sys.error("RootWriter doesn't have a parent")

  private var finished = false
  protected def entity = rootEntity

  override def startObject(): ObjectWriter = {
    if (finished) sys.error("RootWriter already started. Cannot start a new object!")
    else new ObjectWriter(entity, this, "")
  }

  override def endObject() = { finished = true; self }
}

// On ending this object, the parent is returned.
private[writers] class ObjectWriter(val container: PropertyContainer, val parent: DSWriter, prefix: String) extends DSWriter { self =>

  def endObject(): DSWriter = parent

  def endArray(): DSWriter = error("endArray")

  override def handleVal(value: Any): DSWriter = error("handleVal")

  def startObject(): ObjectWriter = error("startObject")

  def startArray() = error("startArray")

  override def startField(name: String) = new FieldWriter(self, if(self.prefix == "") name else self.prefix + "." + name)
}

private[writers] class FieldWriter(val parent: ObjectWriter, prefix: String) extends DSWriter with GAEWriteHandler { self =>

  def endObject(): DSWriter = error("endObject")

  def endArray(): DSWriter = error("endArray")

  def container = parent.container

  override def startObject(): ObjectWriter = {
    new ObjectWriter(container, parent, prefix)
  }

  override def startArray(): ArrayWriter = new ArrayWriter(self)

  override def handleVal(value: Any): DSWriter = {
    container.setProperty(prefix, handleGAEVal(value))
    parent
  }
}

// on ending the array, the array is added using parent.handleVal and the reader returned should be the underlying reader
private[writers] class ArrayWriter(val parent: DSWriter) extends DSWriter with GAEWriteHandler { self =>

  private val arr = new java.util.LinkedList[Any]()

  def endObject(): DSWriter = error("endObject")

  def endArray(): DSWriter = parent.handleVal(arr)

  protected def container: PropertyContainer = error("container")

  def startObject(): ObjectWriter = {
    val container = new EmbeddedEntity()
    handleVal(container)
    new ObjectWriter(container, self, "")
  }

  def startArray(): ArrayWriter = new ArrayWriter(self)

  override def handleVal(value: Any): DSWriter = {
    arr.add(handleGAEVal(value))
    self
  }
}

// Just a helper that filters types for GAE
trait GAEWriteHandler {
  def handleGAEVal(value: Any): Any = value match {
    case s: String => if(s.length > 500) new Text(s) else s
    case b: Array[Byte] => if (b.length > 500) new Blob(b) else new ShortBlob(b)
    case value => value
  }
}


