package scalads.macroimpls.macrohelpers

import scala.reflect.macros.Context
import java.util.Date

import scalads.annotations.Rename


class MacroHelpers[CTPE <: Context](val c: CTPE) {

  import c.universe._

  def macroError(msg: String) = { c.error(c.enclosingPosition, msg); throw new Exception }

  def getNameOption(sym: Symbol): Option[String] = sym.annotations.collect {
    case Annotation(tpe, Literal(Constant(name: String))::Nil, _) if tpe =:= typeOf[Rename] => name
  }.headOption

  // For building objects that take type parameters
  def typeArgumentTree(t: c.Type): c.Tree = t match {
    case TypeRef(_, _, typeArgs @ _ :: _) => AppliedTypeTree(
          Ident(t.typeSymbol), typeArgs map (t => typeArgumentTree(t)) )
    case _                                => Ident(t.typeSymbol.name)
  }

  def classNameExpr(tpe: Type) = c.Expr[String]{
    // println(tpe.typeSymbol.fullName)
    Literal(Constant(tpe.typeSymbol.fullName))
  }

  def mkList[T](things: List[c.Expr[T]]): c.Expr[List[T]] = {
    c.Expr[List[T]](Apply(Ident(newTermName("List")), things.map(_.tree)))
  }

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
      c.typeOf[Array[Byte]]::
      c.typeOf[scala.Symbol]::Nil
  }

  def isPrimitive(tpe: c.Type) = primitiveTypes.exists(tpe =:= _)

  private def constructorExtractor(tree: Tree): List[List[Tree]] = {
    def extract(tree: Tree, lst: List[List[Tree]]): List[List[Tree]] = tree match {
      case Apply(tree, args: List[_]) => extract(tree, args::lst)
      case _ => lst
    }
    extract(tree, Nil)
  }

  def buildObjParamExtract(tree: Tree) = {
    val Block(reader::newObjTree::Nil, _) = tree
    val ValDef(_, _, _, objConstructor) = newObjTree

    (constructorExtractor(objConstructor), reader)
  }

}
