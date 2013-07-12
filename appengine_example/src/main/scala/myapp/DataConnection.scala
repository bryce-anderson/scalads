package myapp

import scalads.appengine.GAEDatastore

/**
 * @author Bryce Anderson
 *         Created on 7/10/13
 */
trait DataConnection {
  def ds = GAEDatastore.getDatastoreService()
}
