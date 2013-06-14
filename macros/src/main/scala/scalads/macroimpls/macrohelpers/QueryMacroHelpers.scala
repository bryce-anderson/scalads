package scalads.macroimpls.macrohelpers

import scala.reflect.macros.Context
import scalads.macroimpls.macrohelpers
import scalads.readers.ObjectReader

/**
 * @author Bryce Anderson
 *         Created on 5/30/13
 */


class QueryMacroHelpers[CONTEXT <: Context](val c: CONTEXT) {
  import c.universe._
  def findName(tree: Tree, name: Name) = findPathStack(tree, name).mkString(".")

  def findPathStack(tree: Tree, name: Name): List[String] = {
    import c.universe._
    def findName(tree: Tree, stack: List[String]): List[String] = tree match {
      case Select(Ident(inName), otherTree) if inName == name => otherTree.decoded::stack // Finished
      case Select(Ident(inName), otherTree) if inName != name => throw new MatchError(tree)
      case Select(inner, outer) => findName(inner, outer.encoded::stack)
    }
    findName(tree, Nil)
  }

  /** Walks the type tree to find the type of the param specified by the path stack
    *
    * @param tpe    The Type which represents the type you intend to walk
    * @param stack  List of strings which specifies the path to walk
    * @return       The final Type of the param specified by stack
    */
  def findType(tpe: Type, stack: List[String]): Option[Type] = {
    val TypeRef(_, sym: Symbol, tpeArgs: List[Type]) = tpe
    stack.foldLeft[Option[Type]](Some(tpe)){ (t, n) =>
      t.flatMap(_.member(nme.CONSTRUCTOR).asMethod.paramss
        .flatten.find(_.name.decoded == n)
        .map(_.typeSignature.substituteTypes(sym.asClass.typeParams, tpeArgs)))
    }
  }

  def findNameOption(tree: Tree, name: Name, rootTpe: Type): Option[(List[String], Type)] = {
    def findNameOption(tree: Tree, stack: List[String]): Option[(List[String], Type)] = tree match {
      case Select(Ident(inName), otherTree) if inName == name =>
        val fullStack = otherTree.decoded::stack
        findType(rootTpe, fullStack).map((fullStack, _))
      case Select(Ident(inName), otherTree) if inName != name => None
      case Select(inner, outer) => findNameOption(inner, outer.encoded::stack)
      case _ => None
    }
    findNameOption(tree, Nil)
  }

  def getPathStacks(tpe: Type): List[(List[String], Type)]  = {
    val helpers = new macrohelpers.MacroHelpers[c.type](c)
    def getStacks(tpe: Type, stack: List[String]): List[(List[String], Type)] = {
      val TypeRef(_, sym: Symbol, tpeArgs: List[Type]) = tpe
      tpe.member(nme.CONSTRUCTOR).asMethod.paramss.flatten.flatMap { pSym =>
        val tpe = pSym.typeSignature.substituteTypes(sym.asClass.typeParams, tpeArgs)
        if (helpers.isPrimitive(tpe)) {
          List(((pSym.name.decoded::stack).reverse, tpe))
        } else { getStacks(tpe, pSym.name.decoded::stack) }
      }
    }
    getStacks(tpe, Nil)
  }

  def getReaderAndName(reader: c.Expr[ObjectReader], stack: List[String]): (c.Expr[ObjectReader], String) = stack match {
    case str::Nil => (reader, str)
    case h::t =>
      val name = c.literal(h)
      getReaderAndName(reify(reader.splice.getObjectReader(name.splice)), t)
  }
}
