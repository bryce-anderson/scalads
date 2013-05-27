package writers

import java.util.Date
import com.google.appengine.api.datastore.Entity
import com.google.appengine.api.datastore.KeyFactory
import com.google.appengine.api.datastore.Key
import scala.collection.mutable.ListBuffer

/**
 * @author Bryce Anderson
 *         Created on 5/27/13
 */
class GAEDSWriter(valTpe: String, parentKey: Key = null) extends Writer[List[Entity]] {

  private val entities = new ListBuffer[Entity]
  private val rootEntity = new Entity(valTpe, parentKey)
  private val ef = { (objType: String, parent: Key) =>
    val e = new Entity(objType, parent)
    entities += e
    e
  }
  private var writer: DSWriter = new ObjectWriter(rootEntity, null, ef)

  def result = entities.result()

  def startArray() {  writer = writer.startArray() }

  def endArray() {  writer = writer.endArray() }

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
  final type EntityFactory = (String, Key) => Entity

  protected def parent: DSWriter
  protected def entity: Entity

  private[writers] def handleVal(value: Any): DSWriter = error("handleVal")

  protected def ef: EntityFactory = parent.ef

  protected final def error(op: String) = sys.error(s"Writer ${this.getClass.toString} cannot perform $op")

  def startArray(): DSWriter = {
    val e = ef("array", entity.getKey)
    new ArrayWriter(e, self, ef)
  }

  def endArray(): DSWriter = parent

  def startObject(objType: String = "object"): DSWriter = {
    val e =ef(objType, entity.getKey)
    new ObjectWriter(e, self, ef)
  }

  def endObject(): DSWriter = parent

  def startField(name: String): DSWriter = error("startField")
}

private[writers] class ObjectWriter(val entity: Entity, val parent: DSWriter, ef: DSWriter#EntityFactory) extends DSWriter { self =>
  override def handleVal(value: Any): DSWriter = error("handleVal")
  override def startField(name: String) = new DSWriter {
    def entity = null
    def parent = self
    override def handleVal(value: Any): DSWriter = {
      self.entity.setProperty(name, value)
      self
    }
  }
}

private[writers] class ArrayWriter(val entity: Entity, val parent: DSWriter, ef: DSWriter#EntityFactory) extends DSWriter { self =>
  private var index = 0
  override def handleVal(value: Any): DSWriter = {
    val e = ef(value.getClass.toString, entity.getKey)
    e.setProperty("index", index)
    index+=1
    e.setProperty("value", value)
    self
  }
}

