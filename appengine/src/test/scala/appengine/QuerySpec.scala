package appengine

import com.google.appengine.api.datastore.{Query => GQuery, Entity}
import com.google.appengine.api.datastore.FetchOptions.Builder._
import java.util.Date
import scalads.core.{QueryIterator, EntityBacker}
import scalads.appengine.GAEDatastore

/**
 * @author Bryce Anderson
 *         Created on 5/30/13
 */
class QuerySpec extends GAESpecTemplate  {

  val ds = GAEDatastore.getDatastoreService()

  def addTests  = {
    0 until 10 foreach { i =>
      val test = Test(i, "test " + i)
      ds.put(test)
    }
  }

  case class  Test(in: Int, in2: String)
  case class Compound(in: Int, in2: Test)
  case class Types(in1: Int, in2: Long, in3: Float, in4: Double, in5: String, in6: Date)

  "Query"  should "compile filters"  in {
    val query = ds.query[Test]
        .filter{ bryce => bryce.in2 > "sweet"
    }

    query.filter { bryce =>
      "sweet" < bryce.in2
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

    val test =  Test(1, "two")
    query.filter{ bryce =>  bryce.in < test.in }

    query.filter{ bryce => test.in < bryce.in }
  }

  it should  "produce an iterator" in {

    addTests

    ds.svc.prepare(new GQuery(classOf[Test].getName)).countEntities(withLimit(100)) should equal (10)

    val results = ds.query[Test]
      .filter(_.in > 0)
      .sortAsc(_.in)
      .sortDec(_.in2)
      .getIterator.toList

    results.length should equal (9)
  }

  it should "work with FetchOptions" in {
    addTests

    val results = ds.query[Test]
      .limit(3)
      .getIterator.toList

    results.length should equal (3)

    val results2: QueryIterator[Test with EntityBacker[Test, Entity], Entity] = ds.query[Test]
      .limit(3)
      .getIterator

    results2.length should equal (3)
  }

  it should "do projections properly" in {
    addTests

    val results1 = ds.query[Test]
          .project{ i => (i.in, i.in2) }

    results1.length should equal(10)

    val results2 = ds.query[Test]
      .project( i => i.in )
    results2.toList should equal((0 until 10).toList)

  }

  it should "project types properly" in {
    val date = new Date()
    val types =  Types(1, 2, 3.0f, 4, "five", date)
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

  it should "project compound objects" in {
    val comp = Compound(1, Test(1, "one"))
    ds.put(comp)

    val result = ds.query[Compound]
      .project( i => (i.in, i.in2.in))
      .next()

    result._1 should equal(comp.in)
    result._2 should equal(comp.in2.in)
  }

  it should "project compound objects with modifiers" in {
    val comp = Compound(1, Test(1, "one"))
    ds.put(comp)

    val result1 = ds.query[Compound]
      .project( i => (i.in, i.in2.in*4))
      .next()

    result1._1 should equal(comp.in)
    result1._2 should equal(comp.in2.in*4)

    val result2 = ds.query[Compound]
      .project{i =>
      val a = i.in
      val b = 34
      (a, i.in2.in*b)
    }.next()

    result2._1 should equal(comp.in)
    result2._2 should equal(comp.in2.in*34)
  }

  it should "allow intermediate objects" in {
    case class SComp(comp1: Int, comp2: Compound)
    val comp = SComp(22, Compound(1, Test(1, "one")))
    ds.put(comp)

    val result1 =  ds.query[SComp]
      .project{ i =>
      val a: Test = i.comp2.in2
      (i.comp2.in, a.in*12)
      }.next()

    result1._1 should equal(comp.comp2.in)
    result1._2 should equal(comp.comp2.in2.in*12)
  }

  it should "allow intermediate lists" in {
    case class WithList(number: Int, list: List[Int])
    val wl = WithList(1, 1::2::3::Nil)
    ds.put(wl)

    val result1 =  ds.query[WithList]
      .project{ i =>
      val lst = i.list
      (lst.head, lst.tail)
    }.next()

    result1._1 should equal(1)
    result1._2 should equal(2::3::Nil)
  }
}
