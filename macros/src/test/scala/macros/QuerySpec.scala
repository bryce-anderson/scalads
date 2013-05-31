package macros

import com.google.appengine.api.datastore.Query.Filter

/**
 * @author Bryce Anderson
 *         Created on 5/30/13
 */
class QuerySpec extends GAESpecTemplate {

  case class Test(in: Int, in2: String)

  class Query[U](filter: Filter) extends util.Query[U] {
    def addFilter(filter: Filter): util.Query[U] = new Query(filter)
  }

  "Query" should "work" in {
    val query = new Query[Test](null)
    query.filter{ bryce =>
      bryce.in2 > "sweet"
    }

    query.filter{ bryce =>
      "sweet" < bryce.in2 // bryce.in > 0 && bryce.in < -1 ||
    }
  }

  it should "deal with non constants" in {
    val query = new Query[Test](null)
    val test = Test(1, "two")
    query.filter{ bryce =>
      bryce.in < test.in
        }

    query.filter{ bryce =>
      test.in < bryce.in
    }
  }

}
