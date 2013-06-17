package appengine

import org.scalatest.{BeforeAndAfterAll, FlatSpec}
import org.scalatest.matchers.ShouldMatchers
import com.google.appengine.tools.development.testing.{LocalDatastoreServiceTestConfig, LocalServiceTestHelper}

/**
 * @author Bryce Anderson
 *         Created on 6/16/13
 */
trait GAESpecTemplate extends FlatSpec with BeforeAndAfterAll with ShouldMatchers {
  private val helper = new LocalServiceTestHelper(new LocalDatastoreServiceTestConfig())
  helper.setUp()

  override def afterAll() {
    //GAESpecTemplate.tearDown(helper)
    helper.tearDown()
    super.afterAll()
  }

  override def beforeAll() {
    //GAESpecTemplate.setUp(helper)
    //helper.setUp()
    super.beforeAll()
  }
}
