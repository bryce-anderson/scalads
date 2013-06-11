package scalads

import language.experimental.macros
import scala.reflect.macros.Context

import scala.collection.mutable.ListBuffer
import scalads.readers.ObjectReader
import macroimpls.Serializer


import scalads.core._
import scalads.writers.Writer
import scala.reflect.ClassTag

/**
 * @author Bryce Anderson
 *         Created on 5/31/13
 */
trait AbstractDatastore[Key, Entity] { self =>

  type QueryType[U] <: Query[U, Entity]

  def withTransaction[U](f: => U): U

  def delete(entity: Entity): Unit

  def delete(entity: EntityBacker[_, Entity]):Unit = delete(entity.ds_entity)

  def update[U](it: QueryIterator[U with EntityBacker[U, Entity], Entity])(f: U => Option[U]) {
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

  // Takes a method that will operate on the writer
  def putRaw(tpe: ClassTag[_], parent: Key)( f: Writer[Any] => Unit): Key

  def mapQuery[U](clazz: ClassTag[U])(f: (AbstractDatastore[_, Entity], ObjectReader) => U with EntityBacker[U, Entity]): QueryType[U]

  def query[U](implicit clazz: ClassTag[U]): QueryType[U] = macro AbstractDatastore.queryImpl[U, Entity, QueryType[U]]

  def put[U](obj: U)(implicit clazz: ClassTag[U]): Key = macro AbstractDatastore.putImplNoKey[U, Key, Entity]
}

object AbstractDatastore {

  def queryImpl[U: c.WeakTypeTag, E: c.WeakTypeTag, Q <: Query[U, E]]
  (c: Context { type PrefixType <: AbstractDatastore[_, E] })(clazz: c.Expr[ClassTag[U]]): c.Expr[Q] = {
    import c.universe._

    val deserializeExpr = macroimpls.EntityDeserializer.extendWithEntityBacker[U, E](c)(
      c.Expr[AbstractDatastore[_, E]](Ident(newTermName("ds"))),
      c.Expr[ObjectReader](Ident(newTermName("reader")))
    )

    val result = reify (
      c.prefix.splice.mapQuery(clazz.splice){(ds, reader) =>
        deserializeExpr.splice
      }
    )
    //println(result)
    result.asInstanceOf[c.Expr[Q]]
  }

  def putImplNoKey[U: c.WeakTypeTag, K, E](c: Context {type PrefixType = AbstractDatastore[K, E] })
                                          (obj: c.Expr[U])(clazz: c.Expr[ClassTag[U]]): c.Expr[K] =
    putImpl[U, K, E](c)(obj, c.universe.reify(null).asInstanceOf[c.Expr[K]])(clazz)

  def putImpl[U: c.WeakTypeTag, K, E](c: Context { type PrefixType = AbstractDatastore [K, E]})
                    (obj: c.Expr[U], parent: c.Expr[K])(clazz: c.Expr[ClassTag[U]]) = {
    import c.universe._

    val serializeExpr = Serializer.serializeImpl[U](c)(obj, c.Expr[Writer[Any]](Ident(newTermName("writer"))))

    reify(
      c.prefix.splice.putRaw(clazz.splice, parent.splice){ writer: Writer[Any] => serializeExpr.splice }
    )
  }
}
