package scalads.macroimpls

import scalads.writers.Writer

import language.experimental.macros
import scala.reflect.macros.Context

/**
 * @author Bryce Anderson
 *         Created on 6/14/13
 */

trait EntitySerializer[U] {
  def serialize[A](obj: U, writer: Writer[A]): Writer[A]
}

object EntitySerializer {
  implicit def getSerializer[U] = macro impl[U]

  def impl[U: c.WeakTypeTag](c: Context): c.Expr[EntitySerializer[U]] = {
    import c.universe._

    val objExpr = c.Expr[U](Ident(newTermName("obj")))
    val writerExpr = c.Expr[Writer[Any]](Ident(newTermName("writer")))
    val clazzExpr = c.Expr[Class[U]](Literal(Constant(weakTypeOf[U])))
    val serializeExpr = Serializer.serializeImpl[U](c)(objExpr, writerExpr)

    reify ( new EntitySerializer[U] {
      def serialize[A](obj: U, writer: Writer[A]): Writer[A] = {
        serializeExpr.splice
        writer
      }

      def clazz: Class[U] = clazzExpr.splice
    })
  }
}
