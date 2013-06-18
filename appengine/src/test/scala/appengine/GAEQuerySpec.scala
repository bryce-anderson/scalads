package appengine

import scalads.{AbstractDatastore, GenericQuerySpec}
import com.google.appengine.api.datastore.{FetchOptions, Query, Key, Entity}
import scalads.appengine.GAEDatastore
import scalads.annotations.Rename

/**
 * @author Bryce Anderson
 *         Created on 6/16/13
 */
class GAEQuerySpec extends GAESpecTemplate with GenericQuerySpec {
  def ds = GAEDatastore.getDatastoreService()

  def backend: String = "Google App Engine"

  @Rename("foo") case class ClassRenamed(in: Int)

  case class FieldRenamed(@Rename("foo") in: Int)

  "Mongo Specific Queries" should "store renamed types" in {

    ds.put(ClassRenamed(1))

    ds.svc.prepare(new Query("foo")).asList(FetchOptions.Builder.withDefaults()).size() should equal (1)
  }

  it should "find renamed types" in {

    ds.query[ClassRenamed]
      .getIterator.length should equal(1)
  }

  it should "store renamed fields" in {
    import scalads.util.AnnotationHelpers.getName

    ds.put(FieldRenamed(1))

    val a = ds.svc.prepare(new Query("foo")).asList(FetchOptions.Builder.withDefaults())

    a.size() should equal(1)
  }

  it should "find renamed fields" in {
    ds.query[FieldRenamed].getIterator.length should equal(1)
  }

}
