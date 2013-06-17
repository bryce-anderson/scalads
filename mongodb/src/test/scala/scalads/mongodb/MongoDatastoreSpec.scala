package scalads.mongodb

/**
 * @author Bryce Anderson
 *         Created on 6/16/13
 */
class MongoDatastoreSpec extends MongoSpecTemplate {

  case class Test(in: Int, in2: String)
  case class Compound(in: Int, in2: Test)

  "MongoDatastore" should "store a simple object" in {
    val ds = new MongoDatastore(coll)
    ds.put(Test(1, "two"))

    coll.find().toArray.size() should equal (1)
  }

  it should "store a Compound object" in {
    val ds = new MongoDatastore(coll)
    ds.put(Compound(1, Test(1, "two")))

    val arr = coll.find().toArray

    arr.size() should equal (2)
  }
}
