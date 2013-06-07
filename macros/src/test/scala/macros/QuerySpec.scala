package macros

import com.google.appengine.api.datastore.{Query => GQuery}
import com.google.appengine.api.datastore.FetchOptions.Builder._
import java.util.Date
import scalads.Datastore
import scalads.core.{QueryIterator, EntityBacker}

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
  case class Compound(in: Int, in2: Test)
  case class Types(in1: Int, in2: Long, in3: Float, in4: Double, in5: String, in6: Date)

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

    val results2: QueryIterator[Test with EntityBacker[Test]] = ds.query[Test]
      .limit(3)
      .getIterator

    results2.length should equal (3)
  }

  it should "do projections properly" in {
    addTests

    val results = ds.query[Test]
          .limit(3)
          .project{ i => (i.in, i.in2) }

    print(results)
  }

  it should "project types properly" in {
    val date = new Date()
    val types = Types(1, 2, 3.0f, 4, "five",date)
    ds.put(types )

    val result = ds.query[Types]
      .project( i => (i.in1, i.in2, i.in3, i.in4, i.in5, i.in6, i.in5, "cats"))

    val opt = result.next()
    opt._1 should equal(types.in1)
    opt._2 should equal(types.in2)
    opt._3 should equal(types.in3)
    opt._4 should equal(types.in4)
    opt._5 should equal(types.in5)
    opt._6 should equal(types.in6)
    opt._7 should equal(types.in5)
    opt._8 should  equal("cats")
  }

  //class Compound(in: Int, in2: Test)
  it should "project compound objects" in {
    val comp = Compound(1, Test(1, "one"))
    ds.put(comp)

    val result = ds.query[Compound]
      .project( i => (i.in, i.in2.in))
      .next()

    result._1 should equal(comp.in)
    result._2 should equal(comp.in2.in)
  }
}
