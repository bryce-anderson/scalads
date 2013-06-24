package scalads.mongodb

import reactivemongo.api.collections.default.BSONCollection

import concurrent.ExecutionContext.Implicits.global
import reactivemongo.bson.BSONDocument

import scala.concurrent.duration._
import scala.concurrent.Await
import reactivemongo.core.commands.Count

/**
 * @author Bryce Anderson
 *         Created on 6/16/13
 */

class MongoDatastoreSpec extends MongoSpecTemplate {

  case class Test(in: Int, in2: String)
  case class Compound(in: Int, in2: Test)

  "MongoDatastore" should "store a simple object" in {
    val ds = new MongoDatastore(db)
    println("Error: -----------------------------" + Await.result( ds.put(Test(1, "two")), 2.seconds))

    val name = MongoDatastore.collectionName[Test]

    Await.result(
      db[BSONCollection](name)
        .find(BSONDocument())
        .cursor[BSONDocument]
        .toList()
        .map(_.length),
      Duration.Inf) should equal (13)

    Await.result(db.command(Count(name, None)), Duration.Inf) should equal(10)
  }

  it should "store a Compound object" in {
    val ds = new MongoDatastore(db)
    ds.put(Compound(1, Test(1, "two")))

    val name = MongoDatastore.collectionName[Compound]
    val arr = db[BSONCollection](name).find(BSONDocument()).cursor.iterator

    arr.length should equal (1)
  }
}
