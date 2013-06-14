package scalads.macroimpls

import scalads.readers.ObjectReader
import scalads.{macroimpls, AbstractDatastore}
import scalads.core.EntityBacker

import language.experimental.macros
import scala.reflect.macros.Context

/**
 * @author Bryce Anderson
 *         Created on 6/14/13
 */
trait EntityBuilder[U, E] {
  def deserialize(ds: AbstractDatastore[_, E], reader: ObjectReader): U with EntityBacker[U, E]
}

object EntityBuilder {
  implicit def getEntityMaker[U, E]: EntityBuilder[U, E] = macro getEntityMakerImpl[U, E]

  def getEntityMakerImpl[U: c.WeakTypeTag, E: c.WeakTypeTag](c: Context): c.Expr[EntityBuilder[U, E]] = {
    import c.universe._
    val deserializeExpr = macroimpls.EntityDeserializer.extendWithEntityBacker[U, E](c)(
      c.Expr[AbstractDatastore[_, E]](Ident(newTermName("ds"))),
      c.Expr[ObjectReader](Ident(newTermName("reader")))
    )

    val result = reify ( new EntityBuilder[U, E] {
      def deserialize(ds: AbstractDatastore[_, E], reader: ObjectReader): U with EntityBacker[U, E] = {
        deserializeExpr.splice
      }
    })

    result
  }
}