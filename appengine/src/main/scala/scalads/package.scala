/**
 * @author Bryce Anderson
 *         Created on 6/9/13
 */

import com.google.appengine.api.datastore.{Entity => GEntity}
import scalads.appengine.GAEDatastore

package object scalads {

  type Entity = GEntity

}
