package macros

import org.scalatest._
import com.google.appengine.tools.development.testing.{LocalDatastoreServiceTestConfig, LocalServiceTestHelper}
import org.scalatest.matchers.ShouldMatchers
import com.google.appengine.api.datastore.{Entity, Key}

/**
 * @author Bryce Anderson
 *         Created on 5/30/13
 */
class GAESpecTemplate extends FlatSpec with BeforeAndAfter with ShouldMatchers {

  private val helper = new LocalServiceTestHelper(new LocalDatastoreServiceTestConfig())

  after { GAESpecTemplate.tearDown(helper) }

  before {  GAESpecTemplate.setUp(helper) }

}

object GAESpecTemplate {
  private var count = 0
  private val lock = new Object

  def setUp(helper: LocalServiceTestHelper) = lock.synchronized {
    count += 1
    helper.setUp()
  }

  def tearDown(helper: LocalServiceTestHelper) = lock.synchronized {
    count -= 1
    if (count == 0) helper.tearDown()
  }
}