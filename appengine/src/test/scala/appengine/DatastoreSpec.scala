package appengine

import scalads.appengine.GAEDatastore
import com.google.appengine.api.datastore.Entity

/**
 * @author Bryce Anderson
 *         Created on 6/2/13
 */

class DatastoreSpec extends GAESpecTemplate {

  val ds = GAEDatastore.getDatastoreService()

  def addTests = {
    0 until 10 foreach { i =>
      val test = Test(i, "test " + i)
      ds.put(test)
    }
  }

  case class Test(in: Int, in2: String)

  "GAEDatastore" should "perform transactions" in {
    // Will fail
     try (ds.withTransaction {
       val test = Test(1, "two")
       ds.put(test)
       sys.error("Failing...")
     }) catch {
       case t: RuntimeException if t.getMessage == "Failing..." => Unit
     }

    ds.query[Test].getIterator.length should be (0)

    // Should pass
    ds.withTransaction{
      val test = Test(1, "two")
      ds.put(test)
    }

    ds.query[Test].getIterator.length should be (1)
  }

  it should "set parents correctly" in {
    val t1 = new Entity("Junk")
    val t2 = Test(1, "key")
    val t3 = Test(2, "key")
    val parent = ds.putEntity(t1)

    ds.put(t2, parent)
    ds.put(t3)

    ds.query[Test].withParent(parent)
      .getIterator.length should equal (1)

    ds.query[Test]
      .filter(_.in2 == "key")
      .getIterator.length should equal (2)
  }

  it should "update entities" in {

    case class Thing(in: Int)

    0 until 10 map { i => ds.put(Thing(i))}

    val it = ds.query[Thing].getIterator
    ds.update(it){
      case Thing(i) if (i%2 == 0) => Some(Thing(-2))
      case _ => None
    }

    val length = ds.query[Thing]
      .filter(_.in == -2)
      .getIterator.length

    length should equal (5)

  }
}
