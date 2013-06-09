package scalads
package core

import scalads.writers.Writer


/**
 * @author Bryce Anderson
 *         Created on 5/28/13
 */

/** Will be mixed in with the results returned from queries to facilitate updates
  *
  * Naming is ugly so as to not interfere with an entities natural fields/methods
  */

trait EntityBacker[U] { self: U =>
  def ds_serialize(obj: U, entity: Entity): Unit  // Typically generated by the macro
  def ds_backingEntity: Entity                    // Typically generated by the macro
  def ds: Datastore

  def ds_updateEntity = ds_serialize(self, ds_backingEntity)

  def put() = { ds_updateEntity; ds.put(self) }

  def ds_key = ds_backingEntity.getKey

  def ds_id = ds_key.getId

  def ds_parent: Key = ds_key.getParent()

  def ds_setProperty(name: String, value: Any) = ds_backingEntity.setProperty(name, value)
}

trait MongoEntityBacker[U] { self: U =>
  def ds_key: Key

  def ds_setProperty(name: String, value: Any): Unit


}

object EntityBacker {
  implicit def backerToKey(backer: EntityBacker[_]): Key = backer.ds_key
}
