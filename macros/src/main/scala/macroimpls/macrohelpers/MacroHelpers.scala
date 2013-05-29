package macroimpls.macrohelpers

import scala.reflect.macros.Context
import java.util.Date


class MacroHelpers[CTPE <: Context](val c: CTPE) {

  import c.universe._

  def macroError(msg: String) = { c.error(c.enclosingPosition, msg); throw new Exception }
  
  def LIT[B](x: B) = c.Expr[B](Literal(Constant(x)))

  // For building objects that take type parameters
  def typeArgumentTree(t: c.Type): c.Tree = t match {
    case TypeRef(_, _, typeArgs @ _ :: _) => AppliedTypeTree(
          Ident(t.typeSymbol), typeArgs map (t => typeArgumentTree(t)) )
    case _                                => Ident(t.typeSymbol.name)
  }

  def classNameExpr(tpe: Type) = c.Expr[String](
    Select(Select(Ident(newTermName(tpe.typeSymbol.name.decoded)),
    newTermName("getClass")), newTermName("toString"))
  )

  private lazy val primitiveTypes =  {
    c.typeOf[Int]::
      c.typeOf[String]::
      c.typeOf[Char]::
      c.typeOf[Float]::
      c.typeOf[Double]::
      c.typeOf[Boolean]::
      c.typeOf[Long]::
      c.typeOf[Byte]::
      c.typeOf[BigInt]::
      c.typeOf[Short]::
      c.typeOf[BigDecimal]::
      c.typeOf[Date]::
      c.typeOf[scala.Symbol]::Nil
  }

  def isPrimitive(tpe: c.Type) = primitiveTypes.exists(tpe =:= _)
}
