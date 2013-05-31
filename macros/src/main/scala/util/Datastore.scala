package util

import com.google.appengine.api.datastore.{Key, Entity, DatastoreService}

import language.experimental.macros
import scala.reflect.macros.Context

/**
 * @author Bryce Anderson
 *         Created on 5/31/13
 */
object Datastore {

  def put[U](obj: U, ds: DatastoreService): Unit = macro putImpl[U]

  def putImpl[U: c.WeakTypeTag](c: Context)(obj: c.Expr[U], ds: c.Expr[DatastoreService]): c.Expr[Unit] = {
    import c.universe._

    val serializeExpr = macroimpls.Serializer.serializeObject[U](c)(obj, reify(null: Key))

    reify{
      val e: Entity = serializeExpr.splice
      ds.splice.put(e)
    }
  }
}
