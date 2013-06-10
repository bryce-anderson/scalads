package scalads

import language.experimental.macros
import scala.reflect.macros.Context

import scala.collection.mutable.ListBuffer
import scalads.readers.{ObjectReader, Reader}
import macroimpls.{Serializer, Deserializer}
import macroimpls.macrohelpers.MacroHelpers


import scalads.core._
import scalads.writers.Writer
import scala.reflect.runtime.universe.TypeTag

/**
 * @author Bryce Anderson
 *         Created on 5/31/13
 */
trait AbstractDatastore { self =>

  type Repr <: AbstractDatastore { type Entity = self.Entity }

  type Key <: AnyRef

  type Entity <: AnyRef

  def withTransaction[U](f: => U): U

  def delete(entity: Entity): Unit

  def delete(entity: EntityBacker[_, Entity]):Unit = delete(entity.ds_entity)

  def update[U](it: QueryIterator[U with EntityBacker[U, Entity], Entity, Repr])(f: U => Option[U]) {
    val newEntities = new ListBuffer[Entity]
    it.foreach { i =>
      f(i).foreach{ r =>
          i.ds_serialize(r, newWriter(i.ds_entity))
          newEntities += i.ds_entity
      }
    }
    put(newEntities.toList: Iterable[Entity])
  }

  def put(obj: EntityBacker[_, Entity]) {
    obj.ds_updateEntity
    putEntity(obj.ds_entity)
  }

  def newReader(entity: Entity): ObjectReader

  def newWriter(entity: Entity): Writer[Entity]

  def put(entities: Iterable[Entity]): Unit

  def putEntity(entity: Entity): Key

  def put(parent: Key, f: Writer[Any] => Unit): Key     // Takes a method that will operate on the writer

  def mapQuery[U](tpe: String)(f: (Repr, ObjectReader) => U with EntityBacker[U, Entity]): Query[U, Entity, Repr]

  //def query[U]: Query[U, Entity] = macro AbstractDatastore.queryImpl[U, Entity]

  def put[U](obj: U): Key = macro AbstractDatastore.putImplNoKey[U, Key, Entity]
}

object AbstractDatastore {
  //def getDatastoreService() = new AbstractDatastore(DatastoreServiceFactory.getDatastoreService)

  def queryImpl[U: c.WeakTypeTag, E: c.WeakTypeTag, Q <: Query[U, E, DS]: c.WeakTypeTag, DS <: AbstractDatastore {type Entity = E}: c.WeakTypeTag]
  (c: Context { type PrefixType <: AbstractDatastore { type Entity = E } }): c.Expr[Q] = {

    val helpers = new MacroHelpers[c.type](c)
    import c.universe._

    // val nameExpr = helpers.classNameExpr(weakTypeOf[U])
    val deserializeExpr = macroimpls.EntityDeserializer.extendWithEntityBacker[U, E, DS](c)(
      c.Expr[DS](Ident(newTermName("ds"))),
      c.Expr[ObjectReader](Ident(newTermName("reader")))
    )
    val typeID = c.literal(weakTypeOf[U].typeSymbol.fullName)
    val result = reify (
      c.prefix.splice.mapQuery(typeID.splice){(ds, reader) =>
        deserializeExpr.splice
      }
    )
    println(result)
    result.asInstanceOf[c.Expr[Q]]
  }

//  def putImplBacked[U: c.WeakTypeTag, E](c: Context {type PrefixType = AbstractDatastore})(obj: c.Expr[U], parent: c.Expr[EntityBacker[_, E]]): c.Expr[Key] =
//    putImpl[U](c)(obj, c.universe.reify(parent.splice.ds_key))

  def putImplNoKey[U: c.WeakTypeTag, K, E](c: Context {type PrefixType = AbstractDatastore { type Key = K; type Entity = E }})
                                          (obj: c.Expr[U]): c.Expr[K] =
    putImpl[U, K, E](c)(obj, c.universe.reify(null).asInstanceOf[c.Expr[K]])

  def putImpl[U: c.WeakTypeTag, K, E](c: Context {
    type PrefixType = AbstractDatastore { type Key = K; type Entity = E }
  })(obj: c.Expr[U], parent: c.Expr[K]) = {
    import c.universe._

    val serializeExpr = Serializer.serializeImpl[U](c)(obj, c.Expr[Writer[Any]](Ident(newTermName("writer"))))

    reify(
      c.prefix.splice.put( parent.splice, { writer: Writer[Any] => serializeExpr.splice })
    )
  }
}
