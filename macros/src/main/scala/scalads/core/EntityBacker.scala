package scalads
package core

import scalads.writers.Writer
import com.sun.corba.se.impl.orbutil.ObjectWriter


/**
 * @author Bryce Anderson
 *         Created on 5/28/13
 */

/** Will be mixed in with the results returned from queries to facilitate updates
  *
  * Naming is ugly so as to not interfere with an entities natural fields/methods
  */

trait EntityBacker[U, E] { self: U =>

  def ds_serialize(obj: U, writer: Writer[_]): Writer[_]  // Typically generated by the macro

  def ds: AbstractDatastore[_, E]

  def ds_entity: E

  def ds_update(): Unit = {
    ds_serialize(self, ds.newWriter(ds_entity))
    ds.put(self)
  }

  def ds_remove() = { ds.delete(self) }

}
