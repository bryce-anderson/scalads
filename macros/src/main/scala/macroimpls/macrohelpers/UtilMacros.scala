package macroimpls.macrohelpers

import writers.GAEDSWriter
import language.experimental.macros
import scala.reflect.macros.Context
import com.google.appengine.api.datastore.{Entity, Key}

/**
 * @author Bryce Anderson
 *         Created on 5/31/13
 */
object UtilMacros {

  def GAEDSWriterImplKey[U: c.WeakTypeTag](c: Context)(parent: c.Expr[Key]): c.Expr[GAEDSWriter] = {
    import c.universe._

    val tpe = weakTypeOf[U]
    val name = c.Expr[String](Literal(Constant(tpe.typeSymbol.fullName)))

    reify(new GAEDSWriter(new Entity(name.splice, parent.splice)))
  }

  def GAEDSWriterImplEntity[U: c.WeakTypeTag](c: Context)(parent: c.Expr[Entity]): c.Expr[GAEDSWriter] =
  GAEDSWriterImplKey(c)(c.universe.reify(parent.splice.getKey))

  def GAEDSWriterImplBare[U: c.WeakTypeTag](c: Context): c.Expr[GAEDSWriter] =
    GAEDSWriterImplKey(c)(c.universe.reify(null: Key))

}
