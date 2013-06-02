package util

import com.google.appengine.api.datastore.{Key, Entity, DatastoreService, DatastoreServiceFactory}
import collection.JavaConverters._

import language.experimental.macros
import scala.reflect.macros.Context
import macroimpls.QueryMacros

/**
 * @author Bryce Anderson
 *         Created on 5/31/13
 */
class Datastore(val ds: DatastoreService) {

  def withTransaction[U](f: => U): U = {
    val txn = ds.beginTransaction()
    try { val a = f; txn.commit(); a }
    finally { if(txn.isActive) txn.rollback() }
  }

  def delete(entity: Entity):Unit = ds.delete(entity.getKey)

  def delete(entity: EntityBacker):Unit = delete(entity.ds_backingEntity)

  def put(obj: EntityBacker) {
    obj.ds_updateEntity
    put(obj.ds_backingEntity)
  }

  def put(entity: Entity): Unit = ds.put(entity)


  def put(objs: Iterable[EntityBacker]) {
    ds.put(objs.map{e => e.ds_updateEntity; e.ds_backingEntity}.asJava)
  }

  def query[U]: Query[U] = macro Datastore.queryImpl[U]

  def put[U](obj: U) = macro Datastore.putImplNoKey[U]

  def put[U](obj: U, parent: Key): Key = macro Datastore.putImpl[U]

  def put[U](obj: U, parent: EntityBacker) = macro Datastore.putImplBacked[U]
}

object Datastore {
  def getDatastoreService() = new Datastore(DatastoreServiceFactory.getDatastoreService)

  def queryImpl[U: c.WeakTypeTag](c: Context { type PrefixType = Datastore }): c.Expr[Query[U]] =
    QueryMacros.ObjApplyImpl[U](c)(c.universe.reify(c.prefix.splice.ds))

  def putImplBacked[U: c.WeakTypeTag](c: Context {type PrefixType = Datastore})(obj: c.Expr[U], parent: c.Expr[EntityBacker]): c.Expr[Key] =
    putImpl[U](c)(obj, c.universe.reify(parent.splice.ds_key))

  def putImplNoKey[U: c.WeakTypeTag](c: Context {type PrefixType = Datastore})(obj: c.Expr[U]): c.Expr[Key] =
    putImpl[U](c)(obj, c.universe.reify(null: Key))

  def putImpl[U: c.WeakTypeTag](c: Context {type PrefixType = Datastore})(obj: c.Expr[U], parent: c.Expr[Key]): c.Expr[Key] = {
    import c.universe._

    val serializeExpr = macroimpls.Serializer.serializeObject[U](c)(obj, parent)

    reify{
      val e: Entity = serializeExpr.splice
      c.prefix.splice.ds.put(e)
    }
  }
}
