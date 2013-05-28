package macros

import org.scalatest._
import writers.GAEDSWriter
import com.google.appengine.tools.development.testing.{LocalDatastoreServiceTestConfig, LocalServiceTestHelper}
import org.scalatest.matchers.ShouldMatchers


/**
 * @author Bryce Anderson
 *         Created on 5/27/13
 */

class SerializerSpec extends FlatSpec with BeforeAndAfter with ShouldMatchers {

  val helper = new LocalServiceTestHelper(new LocalDatastoreServiceTestConfig())

  after { helper.tearDown() }

  before {  helper.setUp() }

  case class Simple(in: Int, in2: String)

  "DSWriter" should  "Write a simple entity" in {

    val a = Simple(0, "one")
    val writer = new GAEDSWriter("Simple")
    macroimpls.Serializer.serialize(a, writer)
    println(writer.result)

    (4) should equal (4)
  }

  it should "Store a compound object" in {
    case class Compound(name: String, simple: Simple, end: String)

    val a = Compound("compound", Simple(0, "one"), "the end")
    val writer = new GAEDSWriter("Compound")
    macroimpls.Serializer.serialize(a, writer)
    println(writer.result.map{e => e.getParent})

    (4) should equal (4)
  }
}
