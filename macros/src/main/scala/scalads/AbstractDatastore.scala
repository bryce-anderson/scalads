package scalads

import language.experimental.macros
import scala.reflect.macros.Context

import collection.JavaConverters._

import scala.collection.mutable.ListBuffer
import scalads.readers.{ObjectReader, Reader}
import macroimpls.{Serializer, Deserializer}
import macroimpls.macrohelpers.MacroHelpers


import scalads.core._
import scalads.writers.Writer
import scalads.core.Key

/**
 * @author Bryce Anderson
 *         Created on 5/31/13
 */
trait AbstractDatastore[E] {

  type QueryT[_] <: AbstractQuery[_]

  def withTransaction[U](f: => U): U

  def delete(entity: E): Unit

  def delete(entity: EntityBacker[_, E]):Unit = delete(entity.ds_entity)

  def update[U](it: QueryIterator[U with EntityBacker[U, E], E])(f: U => Option[U]) {
    val newEntities = new ListBuffer[E]
    it.foreach { i =>
      f(i).foreach{ r =>
          i.ds_serialize(r, i.makeWriter(i.ds_entity))
          newEntities += i.ds_entity
      }
    }
    put(newEntities.toList: Iterable[E])
  }

  def put(obj: EntityBacker[_, E]) {
    obj.ds_updateEntity
    put(obj.ds_entity)
  }

  def put(entities: Iterable[E]): Unit

  def put(entity: E): Key

  def put(parent: Key, f: Writer[_] => Unit): Key     // Takes a method that will operate on the writer

  def mapQuery[U](f: (AbstractDatastore, ObjectReader) => U with EntityBacker[U, E])

  def query[U]: QueryT[U] = macro AbstractDatastore.queryImpl[U]

  def put[U](obj: U): Key = macro AbstractDatastore.putImplNoKey[U]
}

object AbstractDatastore {
  //def getDatastoreService() = new AbstractDatastore(DatastoreServiceFactory.getDatastoreService)

  def queryImpl[U: c.WeakTypeTag, E](c: Context { type PrefixType = AbstractDatastore }): c.Expr[Query[U]] = {
    val helpers = new MacroHelpers[c.type](c)

    import c.universe._

    // val nameExpr = helpers.classNameExpr(weakTypeOf[U])
    val deserializeExpr = macroimpls.EntityDeserializer.extendWithEntityBacker[U, E](c)(
      c.Expr[AbstractDatastore](Ident(newTermName("ds"))),
      c.Expr[ObjectReader](Ident(newTermName("reader")))
    )
    reify (
      c.prefix.splice.mapQuery{(ds, reader) =>
        deserializeExpr.splice
      }
    )
  }

//  def putImplBacked[U: c.WeakTypeTag, E](c: Context {type PrefixType = AbstractDatastore})(obj: c.Expr[U], parent: c.Expr[EntityBacker[_, E]]): c.Expr[Key] =
//    putImpl[U](c)(obj, c.universe.reify(parent.splice.ds_key))

  def putImplNoKey[U: c.WeakTypeTag, K](c: Context {type PrefixType = AbstractDatastore})(obj: c.Expr[U]): c.Expr[Key[K]] =
    putImpl[U](c)(obj, c.universe.reify(null: Key[K]))

  def putImpl[U: c.WeakTypeTag](c: Context {type PrefixType = AbstractDatastore})(obj: c.Expr[U], parent: c.Expr[Key]): c.Expr[Key] = {
    import c.universe._

    val serializeExpr = Serializer.serializeImpl[U](c)(obj, c.Expr[Writer[_]](Ident(newTermName("writer"))))

    reify(
      c.prefix.splice.put( parent.splice, { writer => serializeExpr.splice })
    )
  }
}
