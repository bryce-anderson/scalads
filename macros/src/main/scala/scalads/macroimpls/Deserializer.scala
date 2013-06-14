package scalads.macroimpls

import language.experimental.macros
import scala.reflect.macros.Context

import scalads.readers.ObjectReader



object Deserializer {

  def deserialize[U](reader: ObjectReader): U = macro deserializeImpl[U]

  def deserializeImpl[U: c.WeakTypeTag](c: Context)(reader: c.Expr[ObjectReader]): c.Expr[U] = {
    import c.universe._

    val impl = new DeserializerBase[c.type](c)

    val tpe= weakTypeOf[U]
    val expr = c.Expr[U](impl.reifyObject(tpe, reader))

    //println(expr)  // Debug
    expr
  }
}


