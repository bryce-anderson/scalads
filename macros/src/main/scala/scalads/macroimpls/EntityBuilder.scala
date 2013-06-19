package scalads.macroimpls

import scalads.readers.ObjectReader
import scalads.{macroimpls, AbstractDatastore}
import scalads.core.{Transformer, EntityBacker}

import language.experimental.macros
import scala.reflect.macros.Context

/**
 * @author Bryce Anderson
 *         Created on 6/14/13
 */
trait EntityBuilder[U, E] {
  def deserialize(ds: AbstractDatastore[_, E], trans: Transformer[U, E], entity: E): U with EntityBacker[U, E]
}

object EntityBuilder {
  implicit def getEntityMaker[U, E]: EntityBuilder[U, E] = macro getEntityMakerImpl[U, E]

  def getEntityMakerImpl[U: c.WeakTypeTag, E: c.WeakTypeTag](c: Context): c.Expr[EntityBuilder[U, E]] = {
    import c.universe.{reify, Ident, newTermName}
    val deserializeExpr = macroimpls.EntityDeserializer.extendWithEntityBacker[U, E](c)(
      c.Expr[AbstractDatastore[_, E]](Ident(newTermName("ds"))),
      c.Expr[Transformer[U, E]](Ident(newTermName("trans"))),
      c.Expr[ObjectReader](Ident(newTermName("reader")))
    )

    val result = reify ( new EntityBuilder[U, E] {
      def deserialize(ds: AbstractDatastore[_, E], trans: Transformer[U, E], entity: E): U with EntityBacker[U, E] = {
        val reader = trans.newReader(entity)
        deserializeExpr.splice
      }
    })

    result
  }
}