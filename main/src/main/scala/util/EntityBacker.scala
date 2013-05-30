package util

/**
 * @author Bryce Anderson
 *         Created on 5/28/13
 */

import com.google.appengine.api.datastore.{Key, Entity}

/** Will be mixed in with the results returned from queries to facilitate updates
  *
  * Naming is ugly so as to not interfere with an entities natural fields/methods
  */

trait EntityBacker {
  def ds_backingEntity: Entity

  def ds_key = ds_backingEntity.getKey
  def ds_id = ds_key.getId
  def ds_parent: Key = ds_key.getParent()
}

object EntityBacker {
  implicit def backerToKey(backer: EntityBacker): Key = backer.ds_key
}
