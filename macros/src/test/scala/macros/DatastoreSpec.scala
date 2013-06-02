package macros

import util.Datastore

/**
 * @author Bryce Anderson
 *         Created on 6/2/13
 */
class DatastoreSpec extends GAESpecTemplate {

  val ds = Datastore.getDatastoreService()

  def addTests = {
    0 until 10 foreach { i =>
      val test = Test(i, "test " + i)
      ds.put(test)
    }
  }

  case class Test(in: Int, in2: String)

  "Datastore" should "perform transactions" in {
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

  }
}
