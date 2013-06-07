package scalads.writers

import language.experimental.macros

import java.util.Date
import scalads.Entity
import com.google.appengine.api.datastore.Text

/**
 * @author Bryce Anderson
 *         Created on 5/27/13
 */


class GAEDSWriter(entity: Entity) extends Writer[Entity] {

  private var writer: DSWriter = new RootWriter(entity)

  def result = entity

  def startArray() {  sys.error("GAE Entities don't support Arrays") }

  def endArray() {  sys.error("GAE Entities don't support Arrays") }

  def startObject(objType: String = "object") {  writer = writer.startObject(objType) }

  def endObject() {  writer = writer.endObject() }

  def startField(name: String) {  writer = writer.startField(name) }

  def int(in: Int) { writer = writer.handleVal(in) }

  def string(in: String) { writer = writer.handleVal(in) }

  def float(in: Float) { writer = writer.handleVal(in) }

  def double(in: Double) { writer = writer.handleVal(in) }

  def bigDecimal(in: BigDecimal) { writer = writer.handleVal(in) }

  def bigInt(in: BigInt) { writer = writer.handleVal(in) }

  def short(in: Short) { writer = writer.handleVal(in) }

  def byte(in: Byte) { writer = writer.handleVal(in) }

  def long(in: Long) { writer = writer.handleVal(in) }

  def boolean(in: Boolean) { writer = writer.handleVal(in) }

  def date(in: Date) { writer = writer.handleVal(in) }

  def nil() { writer = writer.handleVal(null) }
}

trait DSWriter { self =>
  protected def parent: DSWriter
  protected def entity: Entity
  def startObject(objType: String = "object"): DSWriter

  private[writers] def handleVal(value: Any): DSWriter = error("handleVal")

  protected final def error(op: String) = sys.error(s"Writer ${this.getClass.toString} cannot perform $op")

  def endObject(): DSWriter = parent

  def startField(name: String): DSWriter = error("startField")
}

class RootWriter(val rootEntity: Entity) extends DSWriter { self =>

  def parent: DSWriter = sys.error("RootWriter doesn't have a parent")

  private var finished = false
  protected def entity = rootEntity

  override def startObject(objType: String): DSWriter = {
    if (finished) sys.error("RootWriter already started. Cannot start a new object!")
    else new ObjectWriter(entity, this, "")
  }

  override def endObject() = { finished = true; self }
}

private[writers] class ObjectWriter(val entity: Entity, val parent: DSWriter, prefix: String) extends DSWriter { self =>
  override def handleVal(value: Any): DSWriter = error("handleVal")

  def startObject(objType: String): DSWriter = error("startObject")

  override def startField(name: String) = new DSWriter {
    def entity = self.entity
    def parent = self

    val prefix = if(self.prefix == "") name else self.prefix + "." + name

    override def startObject(objType: String = "object"): DSWriter = {
      new ObjectWriter(entity, self, prefix)
    }

    override def handleVal(value: Any): DSWriter = {
      value match {
        case s: String => self.entity.setProperty(prefix, if(s.length > 500) new Text(s) else s)
        case value => self.entity.setProperty(prefix, value)
      }
      self
    }
  }
}


