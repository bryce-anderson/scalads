package util

import com.google.appengine.api.datastore._
import collection.JavaConverters._

import language.experimental.macros
import scala.reflect.macros.Context
import macroimpls.Deserializer
import scala.collection.mutable.ListBuffer
import macroimpls.macrohelpers.MacroHelpers
import macro_readers.GAEObjectReader

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

  def delete(entity: EntityBacker[_]):Unit = delete(entity.ds_backingEntity)

  def update[U](it: QueryIterator[U with EntityBacker[U]])(f: U => Option[U]) {
    val newEntities = new ListBuffer[Entity]
    it.foreach { i =>
      f(i) match {
        case Some(r) if r != i =>
          i.ds_serialize(r, i.ds_backingEntity)
          newEntities += i.ds_backingEntity
        case None =>
      }
    }
    put(newEntities.toList: Iterable[Entity])
  }

  def put(obj: EntityBacker[_]) {
    obj.ds_updateEntity
    put(obj.ds_backingEntity)
  }

  def put(entities: Iterable[Entity]): Unit = ds.put(entities.asJava)

  def put(entity: Entity): Key = ds.put(entity)

  def query[U]: Query[U] = macro Datastore.queryImpl[U]

  def put[U](obj: U): Key = macro Datastore.putImplNoKey[U]

  def put[U](obj: U, parent: Key): Key = macro Datastore.putImpl[U]

  def put[U](obj: U, parent: EntityBacker[_]): Key = macro Datastore.putImplBacked[U]
}

object Datastore {
  def getDatastoreService() = new Datastore(DatastoreServiceFactory.getDatastoreService)

  def queryImpl[U: c.WeakTypeTag](c: Context { type PrefixType = Datastore }): c.Expr[Query[U]] = {
    val helpers = new MacroHelpers[c.type](c)

    import com.google.appengine.api.datastore.{Query => GQuery}
    import c.universe._

    val nameExpr = helpers.classNameExpr(weakTypeOf[U])
    val deserializeExpr = Deserializer.deserializeImpl[U](c)(c.Expr[GAEObjectReader](Ident(newTermName("reader"))))
    reify (
      new Query[U](c.prefix.splice, new GQuery(nameExpr.splice),{ entity: Entity =>
        val reader = GAEObjectReader(entity)
        deserializeExpr.splice
      })
    )
  }

  def putImplBacked[U: c.WeakTypeTag](c: Context {type PrefixType = Datastore})(obj: c.Expr[U], parent: c.Expr[EntityBacker[_]]): c.Expr[Key] =
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
