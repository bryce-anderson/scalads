package appengine

import scalads.{AbstractDatastore, GenericQuerySpec}
import com.google.appengine.api.datastore.{Key, Entity}
import scalads.appengine.GAEDatastore

/**
 * @author Bryce Anderson
 *         Created on 6/16/13
 */
class GAEQuerySpec extends GAESpecTemplate with GenericQuerySpec[Key, Entity] {
  def ds: AbstractDatastore[Key, Entity] = GAEDatastore.getDatastoreService()

  def backend: String = "Google App Engine"
}
