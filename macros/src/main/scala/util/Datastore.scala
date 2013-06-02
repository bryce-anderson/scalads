package util

import com.google.appengine.api.datastore.{Key, Entity, DatastoreService}
import collection.JavaConverters._

import language.experimental.macros
import scala.reflect.macros.Context

/**
 * @author Bryce Anderson
 *         Created on 5/31/13
 */
object Datastore {

  def test {
    val a = com.google.appengine.api.datastore.DatastoreServiceFactory.getAsyncDatastoreService
    //a.beginTransaction().
  }

  def withTransaction(f: => Any) = {
    ???
  }

  def put(obj: EntityBacker)(implicit ds: DatastoreService) = {
    obj.ds_updateEntity
    ds.put(obj.ds_backingEntity)
  }

  def put(objs: Iterable[EntityBacker])(implicit ds: DatastoreService) = {

    ds.put(objs.map{e => e.ds_updateEntity; e.ds_backingEntity}.asJava)
  }

  def put[U](obj: U)(implicit ds: DatastoreService): Key = macro putImpl[U]

  def putImpl[U: c.WeakTypeTag](c: Context)(obj: c.Expr[U])(ds: c.Expr[DatastoreService]): c.Expr[Key] = {
    import c.universe._

    val serializeExpr = macroimpls.Serializer.serializeObject[U](c)(obj, reify(null: Key))

    reify{
      val e: Entity = serializeExpr.splice
      ds.splice.put(e)
    }
  }
}
