package scalads.macroimpls

import language.experimental.macros
import scala.reflect.macros.Context
import scalads.readers.{ArrayIterator, ObjectReader}
import scalads.macroimpls.macrohelpers.MacroHelpers
import java.util.Date
import java.text.SimpleDateFormat
import scalads.exceptions.MappingException

/**
 * @author Bryce Anderson
 *         Created on 6/12/13
 */
class DeserializerBase[CONTEXT <: Context](val c: CONTEXT) {

  val helpers = new MacroHelpers[c.type](c)
  import helpers.{isPrimitive, typeArgumentTree, getNameOption}

  import c.universe._

  /* Builds the tree required for the proper from the reader.

    Note that the implementation requires the state of the reader to be that of
    the previous object.

   */
  def buildField(tpe: Type, fieldName: c.Expr[String], reader: c.Expr[ObjectReader]): Tree = {
    if (isPrimitive(tpe)) reifyPrimative(tpe, fieldName, reader)
    // The privileged types
    else if (tpe.erasure <:< typeOf[Option[_]]) {
      reifyOption(tpe, fieldName, reader)
    }
    else if (typeOf[Map[_, _]] <:< tpe.erasure) {
      val orNme = c.fresh("jsonReader$")
      val orExpr = c.Expr[ObjectReader](Ident(newTermName(orNme)))
      val orTree = ValDef(
        Modifiers(),
        newTermName(orNme),
        TypeTree(typeOf[ObjectReader]),
        reader.tree
      )
      Block(orTree::Nil, reifyMap(tpe, orExpr))
    }
    else if (typeOf[List[_]] <:< tpe.erasure) {
      reifyList(tpe, reify{reader.splice.getArrayReader(fieldName.splice)})
    }
    else  reifyObject(tpe, reify{ reader.splice.getObjectReader(fieldName.splice)})
  }

  def reifyString(field: c.Expr[String], reader: c.Expr[ObjectReader]) = reify {
    reader.splice.getString(field.splice)
  }

  def reifySymbol(field: c.Expr[String], reader: c.Expr[ObjectReader]) = reify {
    Symbol(reifyString(field, reader).splice)
  }

  def  reifyOption(tpe: Type, field: c.Expr[String], reader: c.Expr[ObjectReader]):Tree = {
    val TypeRef(_, _, List(argTpe)) = tpe
    val someExpr = c.Expr(buildField(argTpe, field, reader))
    reify{
      try{
        Some(someExpr.splice)
      } catch {
        case _: Throwable => None
      }
    }.tree
  }

  def reifyMap(tpe: Type, reader: c.Expr[ObjectReader]): c.Tree = {
    val TypeRef(_, _, keyTpe::valTpe::Nil) = tpe
    // Capable of parsing maps that contain primitives as keys, not only strings
    val kExpr = c.Expr[String](Ident(newTermName("k")))
    val keyParser = keyTpe match {
      case a if a =:= typeOf[Int]     => reify{kExpr.splice.toInt}
      case a if a =:= typeOf[Long]    => reify{kExpr.splice.toLong}
      case a if a =:= typeOf[Float]   => reify{kExpr.splice.toDouble}
      case a if a =:= typeOf[Double]  => reify{kExpr.splice.toDouble}
      case a if a =:= typeOf[String]  => reify{kExpr.splice}
      case _ => c.abort(c.enclosingPosition, "Map must contain primitive types as keys!")
    }

    val mapVal = c.Expr(buildField(valTpe, kExpr, reader))
    reify {
      reader.splice.getKeys.map{ k =>
        (keyParser.splice, mapVal.splice)
      }.toMap
    }.tree
  }

  def reifyList(tpe: Type, reader: c.Expr[ArrayIterator]): Tree = {
    // builds the cells of a list
    def buildCell(tpe: Type, reader: c.Expr[ArrayIterator]): Tree = {
      if      (tpe =:= typeOf[Int])           reify { reader.splice.nextInt }.tree
      else if (tpe =:= typeOf[Long])          reify { reader.splice.nextLong }.tree
      else if (tpe =:= typeOf[Float])         reify { reader.splice.nextFloat }.tree
      else if (tpe =:= typeOf[Double])        reify { reader.splice.nextDouble }.tree
      else if (tpe =:= typeOf[String])        reify { reader.splice.nextString }.tree
      else if (typeOf[List[_]] <:< tpe.erasure) reifyList(tpe, reify{reader.splice.nextArrayReader})
      else if (typeOf[Map[_, _]] <:< tpe.erasure) {
        val orNme = c.fresh("jsonReader$")
        val orExpr = c.Expr[ObjectReader](Ident(newTermName(orNme)))
        val orTree = ValDef(
          Modifiers(),
          newTermName(orNme),
          TypeTree(typeOf[ObjectReader]),
          reify{reader.splice.nextObjectReader}.tree
        )
        Block(orTree::Nil, reifyMap(tpe, orExpr))
      }
      else reifyObject(tpe, reify{reader.splice.nextObjectReader})
    }

    val TypeRef(_, _, List(argTpe)) = tpe

    val builderExpr = c.Expr[collection.mutable.Builder[Any, List[Any]]](TypeApply(Select(
      Ident(newTermName("List")), newTermName("newBuilder")), List(TypeTree(argTpe))))

    val itNme = c.fresh("jsonIterator$")
    val itExpr = c.Expr[ArrayIterator](Ident(newTermName(itNme)))
    val itTree = ValDef(
      Modifiers(),
      newTermName(itNme),
      TypeTree(typeOf[ArrayIterator]),
      reader.tree
    )

    val cellExpr = c.Expr[Any](buildCell(argTpe, itExpr))
    val itValExpr = c.Expr(itTree)
    reify{
      val builder = builderExpr.splice
      itValExpr.splice
      while(itExpr.splice.hasNext) {
        builder += cellExpr.splice
      }
      builder.result
    }.tree
  }

  def reifyPrimative(tpe: Type, field: c.Expr[String], reader: c.Expr[ObjectReader]) = {
    if      (tpe =:= typeOf[Int])         reify {reader.splice.getInt(field.splice)    }.tree
    else if (tpe =:= typeOf[Short])       reify {reader.splice.getInt(field.splice).asInstanceOf[Short]}.tree
    else if (tpe =:= typeOf[Byte])        reify {reader.splice.getInt(field.splice).asInstanceOf[Byte] }.tree
    else if (tpe =:= typeOf[Long])        reify { reader.splice.getLong(field.splice)  }.tree
    else if (tpe =:= typeOf[Float])       reify { reader.splice.getFloat(field.splice) }.tree
    else if (tpe =:= typeOf[Double])      reify { reader.splice.getDouble(field.splice)}.tree
    else if (tpe =:= typeOf[Boolean])     reify { reader.splice.getBool(field.splice)}.tree
    else if (tpe =:= typeOf[Array[Byte]]) reify { reader.splice.getBytes(field.splice)}.tree
    else if (tpe =:= typeOf[Date])        reify { reader.splice.getDate(field.splice) }.tree
    else if (tpe =:= typeOf[String])      { reifyString(field, reader).tree }
    else if (tpe =:= typeOf[Char])        reify {
      val str = reifyString(field, reader).splice
      if (str.length != 1)
        throw new IllegalStateException(s"String $str is too long to be converted to Char")
      str.charAt(0)
    }.tree
    else if (tpe =:= typeOf[scala.Symbol]) { reifySymbol(field, reader).tree }
    else throw new java.lang.NoSuchFieldException(s"Type '$tpe' is not a primitive!")
  }

  def reifyPrimitiveOpt(tpe: Type, field: c.Expr[String], reader: c.Expr[ObjectReader]): c.Expr[Option[_]] = {
    if      (tpe =:= typeOf[Int])         reify {reader.splice.optInt(field.splice)     }
    else if (tpe =:= typeOf[Short])       reify {reader.splice.optInt(field.splice).map(_.asInstanceOf[Short])}
    else if (tpe =:= typeOf[Byte])        reify {reader.splice.optInt(field.splice).map(_.asInstanceOf[Byte])}
    else if (tpe =:= typeOf[Long])        reify { reader.splice.optLong(field.splice)   }
    else if (tpe =:= typeOf[Float])       reify { reader.splice.optFloat(field.splice)  }
    else if (tpe =:= typeOf[Double])      reify { reader.splice.optDouble(field.splice) }
    else if (tpe =:= typeOf[Boolean])     reify { reader.splice.optBool(field.splice)   }
    else if (tpe =:= typeOf[Array[Byte]]) reify { reader.splice.optBytes(field.splice)  }
    else if (tpe =:= typeOf[Date])        reify { reader.splice.optDate(field.splice)   }
    else if (tpe =:= typeOf[String])      reify { reader.splice.optString(field.splice) }
    else if (tpe =:= typeOf[Char])      reify { reader.splice.optString(field.splice).map{ str =>
      if (str.length != 1)
        throw new IllegalStateException(s"String $str is too long to be converted to Char")
      str.charAt(0)
    } }
    else if (tpe =:= typeOf[Boolean])     reify { reader.splice.optBool(field.splice)   }
    else if (tpe =:= typeOf[Date])         reify { reader.splice.optDate(field.splice) }
    else if (tpe =:= typeOf[scala.Symbol]) reify {
      reader.splice.optString(field.splice).map(Symbol(_))
    }
    else throw new java.lang.NoSuchFieldException(s"Type '$tpe' is not a primitive!")
  }

  // Builds a class and sets its fields if they are detected
  def reifyObject(tpe: Type, reader: c.Expr[ObjectReader]): Tree = {
    // Find some info on our object type
    val TypeRef(_, sym: Symbol, tpeArgs: List[Type]) = tpe
    val newObjTypeTree = typeArgumentTree(tpe)

    // Make the object reader tree bits
    val orNme = c.fresh("reader$")
    val orExpr = c.Expr[ObjectReader](Ident(newTermName(orNme)))
    val orTree = ValDef(
      Modifiers(),
      newTermName(orNme),
      TypeTree(typeOf[ObjectReader]),
      reader.tree
    )

    val objTree: Tree = {
      val ctor = tpe.member(nme.CONSTRUCTOR).asMethod
      if (ctor.alternatives.length > 1)
        c.error(c.enclosingPosition, s"Object of type ${tpe} has multiple constructors and cannot be deserialized")

      ctor.paramss.map(_.zipWithIndex.map {
        case (pSym, index) =>
          // Change out the types if it has type parameters
          val pTpe = pSym.typeSignature.substituteTypes(sym.asClass.typeParams, tpeArgs)
          val fieldName = c.literal(getNameOption(pSym).getOrElse(pSym.name.decoded))

          // If param has defaults, try to find the val in map, or call
          // default evaluation from its companion object
          if (pSym.asTerm.isParamWithDefault && isPrimitive(pTpe) && sym.companionSymbol.isTerm) {
            val fieldExpr = reifyPrimitiveOpt(pTpe, fieldName, orExpr)
            val elseExpr = c.Expr(Select(Ident(sym.companionSymbol), newTermName(
              "$lessinit$greater$default$" + (index+1).toString))
            )
            reify {
              fieldExpr.splice
                .getOrElse(elseExpr.splice)
            }.tree
          } else if (pSym.asTerm.isParamWithDefault && sym.companionSymbol.isTerm) {
            val fieldExpr = c.Expr(buildField(pTpe, fieldName, orExpr))
            val failExpr = c.Expr(Select(Ident(sym.companionSymbol), newTermName(
              "$lessinit$greater$default$" + (index+1).toString))
            )
            reify {
              try {
                fieldExpr.splice // splice in another obj tree
              } catch {
                case e: MappingException =>
                  // Need to use the original symbol.companionObj to get defaults
                  // Would be better to find the generated TermNames if possible
                  failExpr.splice
              }
            }.tree
          } else buildField(pTpe, fieldName, orExpr)
      }).foldLeft[Tree](Select(New(newObjTypeTree), nme.CONSTRUCTOR)){(a ,b) => Apply(a, b) }
    }


    val newObjTerm = newTermName(c.fresh("newObj$"))
    val newObjTree = ValDef(Modifiers(), newObjTerm, newObjTypeTree, objTree)

    Block(orTree::newObjTree::Nil, Ident(newObjTerm))
  }
}
