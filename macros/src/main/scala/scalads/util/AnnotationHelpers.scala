package scalads.util

import reflect.runtime.universe._

import scalads.annotations.Rename

/**
 * @author Bryce Anderson
 *         Created on 6/17/13
 */
object AnnotationHelpers {
  def getName(tg: TypeTag[_]): String = getNameOption(tg.tpe.typeSymbol).getOrElse(tg.tpe.typeSymbol.fullName)

  def getNameOption(sym: Symbol): Option[String] = sym.annotations.collect {
    case Annotation(tpe, Literal(Constant(name: String))::Nil, _) if tpe =:= typeOf[Rename] => name
  }.headOption
}
