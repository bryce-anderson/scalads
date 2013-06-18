package scalads.mongodb

import scalads.annotations.Rename
import com.mongodb.{BasicDBObject}
import scala.reflect.runtime.universe.typeTag

/**
 * @author Bryce Anderson
 *         Created on 6/16/13
 */
class MongoQuerySpec extends MongoSpecTemplate with scalads.GenericQuerySpec {

  lazy val ds = MongoDatastore(coll)

  lazy val backend: String = "Mongo"

  @Rename("foo") case class ClassRenamed(in: Int)

  case class FieldRenamed(@Rename("foo") in: Int)

  "Mongo Specific Queries" should "store renamed types" in {

      ds.put(ClassRenamed(1))

      ds.coll.find(new BasicDBObject(MongoDatastore.dbTypeField, "foo")).count() should equal(1)
  }

  it should "find renamed types" in {

    ds.query[ClassRenamed]
      .getIterator.length should equal(1)
  }

  it should "store renamed fields" in {
    import scalads.util.AnnotationHelpers.getName

    ds.put(FieldRenamed(1))

    val a = ds.coll.find(new BasicDBObject(MongoDatastore.dbTypeField, getName(typeTag[FieldRenamed]))
      .append("foo", 1)
    )

      a.count() should equal(1)
  }

  it should "find renamed fields" in {
    ds.query[FieldRenamed].getIterator.length should equal(1)
  }

}


