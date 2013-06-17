package scalads.mongodb

import java.util.Date
import org.bson.types.ObjectId
import com.mongodb.{WriteResult, DBObject}
import scalads.AbstractDatastore

/**
 * @author Bryce Anderson
 *         Created on 6/16/13
 */
class MongoQuerySpec extends MongoSpecTemplate with scalads.GenericQuerySpec[WriteResult, DBObject] {

  lazy val ds: AbstractDatastore[WriteResult, DBObject] = {
    println(s"Collection: $coll")
    MongoDatastore(coll)
  }
  lazy val backend: String = "Mongo"
//
//  "MongoQuery" should "return simple objects" in {
//     val ds = MongoDatastore(coll)
//
//    ds.put(Test(1, "two"))
//
//    ds.query[Test]
//      .getIterator
//      .length should equal (1)
//
//  }

}


