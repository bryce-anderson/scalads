package macros

import com.google.appengine.api.datastore.{Query => GQuery}
import util.{EntityBacker, Datastore}
import com.google.appengine.api.datastore.FetchOptions.Builder._

/**
 * @author Bryce Anderson
 *         Created on 5/30/13
 */
class QuerySpec extends GAESpecTemplate {

  val ds = Datastore.getDatastoreService()

  def addTests = {
    0 until 10 foreach { i =>
      val test = Test(i, "test " + i)
      ds.put(test)
    }
  }

  case class Test(in: Int, in2: String)

  "Query" should "work" in {
    val query = ds.query[Test]
        .filter{ bryce => bryce.in2 > "sweet"
    }

    query.filter{ bryce =>
      "sweet" < bryce.in2 // bryce.in > 0 && bryce.in < -1 ||
    }

    query.filter(_.in2 < "sweet")
  }

  it should "deal with non constants" in {
    val query = ds.query[Test]
    val test = Test(1, "two")
    query.filter{ bryce =>  bryce.in < test.in }

    query.filter{ bryce => test.in < bryce.in }
  }

  it should "do filtering correctly" in {
    val query = ds.query[Test]
        .filter(_.in < 0)

    val test = Test(1, "two")
    query.filter{ bryce =>  bryce.in < test.in }

    query.filter{ bryce => test.in < bryce.in }
  }

  it should "produce an iterator" in {

    addTests

    ds.ds.prepare(new GQuery("macros.QuerySpec.Test")).countEntities(withLimit(100)) should equal (10)

    val results = ds.query[Test]
      .filter(_.in > 0)
      .sortAscBy(_.in)
      .sortDecBy(_.in2)
      .getIterator.toList

    results.length should equal (9)
  }

  it should "work with FetchOptions" in {

    addTests

    val results = ds.query[Test]
      .limit(3)
      .getIterator.toList

    results.length should equal (3)

    val results2: util.QueryIterator[Test with EntityBacker[Test]] = ds.query[Test]
      .limit(3)
      .getIterator

    results2.length should equal (3)
  }

}
