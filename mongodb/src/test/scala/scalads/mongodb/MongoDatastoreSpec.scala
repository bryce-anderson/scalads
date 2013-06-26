package scalads.mongodb

import reactivemongo.api.collections.default.BSONCollection

import concurrent.ExecutionContext.Implicits.global
import reactivemongo.bson.BSONDocument

import scala.concurrent.duration._
import scala.concurrent.Await

/**
 * @author Bryce Anderson
 *         Created on 6/16/13
 */

class MongoDatastoreSpec extends MongoSpecTemplate {

  case class Test(in: Int, in2: String)
  case class Compound(in: Int, in2: Test)


  "MongoDatastore" should "store a simple object" in {
    val ds = new MongoDatastore(db)

//    (0 until 10) foreach { i => ds.put(Test(i, i.toString))}

    val name = MongoDatastore.collectionName[Test]

    Await.result(
      db[BSONCollection](name)
        .find(BSONDocument())
        .cursor[BSONDocument]
        .toList()
        .map(_.length),
      Duration.Inf) should equal (10)
  }

  it should "store a Compound object" in {
    val ds = new MongoDatastore(db)
//    ds.put(Compound(1, Test(1, "two")))

    val name = MongoDatastore.collectionName[Compound]
    val arr = Await.result(db[BSONCollection](name).find(BSONDocument()).cursor.toList(), Duration.Inf)

    arr.length should equal (1)
  }
}
