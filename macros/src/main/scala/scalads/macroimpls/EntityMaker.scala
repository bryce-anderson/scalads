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
trait EntityMaker[U, E] {
  def deserialize(ds: AbstractDatastore[_, E], reader: ObjectReader): U with EntityBacker[U, E]
}

object EntityMaker {
  implicit def getEntityMaker[U, E]: EntityMaker[U, E] = macro impl[U, E]

  def impl[U: c.WeakTypeTag, E: c.WeakTypeTag](c: Context): c.Expr[EntityMaker[U, E]] = {
    import c.universe._
    val deserializeExpr = macroimpls.EntityDeserializer.extendWithEntityBacker[U, E](c)(
      c.Expr[AbstractDatastore[_, E]](Ident(newTermName("ds"))),
      c.Expr[ObjectReader](Ident(newTermName("reader")))
    )

    val result = reify ( new EntityMaker[U, E] {
      def deserialize(ds: AbstractDatastore[_, E], reader: ObjectReader): U with EntityBacker[U, E] = {
        deserializeExpr.splice
      }
    })

    result
  }
}