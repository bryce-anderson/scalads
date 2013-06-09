package scalads.macroimpls

import language.experimental.macros
import scala.reflect.macros.Context
import macrohelpers.MacroHelpers
import scalads.readers.{ObjectReader, ArrayIterator, GAEObjectReader}
import java.text.SimpleDateFormat
import scalads.exceptions.MappingException

import scalads.{Datastore, Entity}
import scalads.core.EntityBacker


object Deserializer {

  import java.util.Date

  def deserialize[U](reader: GAEObjectReader): U = macro deserializeImpl[U]

  def deserializeImpl[U: c.WeakTypeTag](c: Context)(reader: c.Expr[ObjectReader]): c.Expr[U] = {

    val helpers = new MacroHelpers[c.type](c)
    import helpers.{isPrimitive, typeArgumentTree, macroError, buildObjParamExtract}
    import c.universe._

    def rparseString(field: c.Expr[String], reader: c.Expr[ObjectReader]) = reify {
      reader.splice.getString(field.splice)
    }

    def rparseSymbol(field: c.Expr[String], reader: c.Expr[ObjectReader]) = reify {
      Symbol(rparseString(field, reader).splice)
    }

    def  rparseOption(tpe:Type, field: c.Expr[String], reader: c.Expr[ObjectReader]):Tree = {
      val TypeRef(_, _, List(argTpe)) = tpe
      reify{
        try{
          Some(c.Expr(buildField(argTpe, field, reader)).splice)
        } catch {
          case _: Throwable => None
        }
      }.tree
    }

    def buildMap(tpe:Type, reader: c.Expr[ObjectReader]): c.Tree = {
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

      reify {
        reader.splice.getKeys.map{ k =>
          (keyParser.splice, c.Expr(buildField(valTpe, kExpr, reader)).splice)
        }.toMap
      }.tree
    }

    def buildList(tpe: Type, reader: c.Expr[ArrayIterator]): Tree = {

      // builds the cells of a list
      def buildCell(tpe: Type, reader: c.Expr[ArrayIterator]): Tree = {
        if      (tpe =:= typeOf[Int])           reify { reader.splice.nextInt }.tree
        else if (tpe =:= typeOf[Long])          reify { reader.splice.nextLong }.tree
        else if (tpe =:= typeOf[Float])         reify { reader.splice.nextFloat }.tree
        else if (tpe =:= typeOf[Double])        reify { reader.splice.nextDouble }.tree
        else if (tpe =:= typeOf[String])        reify { reader.splice.nextString }.tree
        else if (typeOf[List[_]] <:< tpe.erasure) buildList(tpe, reify{reader.splice.nextArrayReader})
        else if (typeOf[Map[_, _]] <:< tpe.erasure) {
          val orNme = c.fresh("jsonReader$")
          val orExpr = c.Expr[ObjectReader](Ident(newTermName(orNme)))
          val orTree = ValDef(
            Modifiers(),
            newTermName(orNme),
            TypeTree(typeOf[ObjectReader]),
            reify{reader.splice.nextObjectReader}.tree
          )
          Block(orTree::Nil, buildMap(tpe, orExpr))
        }
        else buildObject(tpe, reify{reader.splice.nextObjectReader})
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

      reify{
        val builder = builderExpr.splice
        c.Expr(itTree).splice
        while(itExpr.splice.hasNext) {
          builder += c.Expr[Any](buildCell(argTpe, itExpr)).splice
        }
        builder.result
      }.tree
    }

    // builds the different fields of an Object or Map
    def buildField(tpe: Type, fieldName: c.Expr[String], reader: c.Expr[ObjectReader]): Tree = {
      if (isPrimitive(tpe)) buildPrimitive(tpe, fieldName, reader)
      // The privileged types
      else if (tpe.erasure <:< typeOf[Option[_]]) {
        rparseOption(tpe, fieldName, reader)
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
        Block(orTree::Nil, buildMap(tpe, orExpr))
      }
      else if (typeOf[List[_]] <:< tpe.erasure) {
        buildList(tpe, reify{reader.splice.getArrayReader(fieldName.splice)})
      }
      else  buildObject(tpe, reify{ reader.splice.getObjectReader(fieldName.splice)})
    }

    def buildPrimitive(tpe: Type, field: c.Expr[String], reader: c.Expr[ObjectReader]) = {
      if      (tpe =:= typeOf[Int])         reify {reader.splice.getInt(field.splice)    }.tree
        // TODO: type Byte and Char
      else if (tpe =:= typeOf[Short])       reify {reader.splice.getInt(field.splice).asInstanceOf[Short]}.tree
      else if (tpe =:= typeOf[Byte])        reify {reader.splice.getInt(field.splice).asInstanceOf[Byte] }.tree
      else if (tpe =:= typeOf[Long])        reify { reader.splice.getLong(field.splice)  }.tree
      else if (tpe =:= typeOf[Float])       reify { reader.splice.getFloat(field.splice) }.tree
      else if (tpe =:= typeOf[Double])      reify { reader.splice.getDouble(field.splice)}.tree
      else if (tpe =:= typeOf[Boolean])     reify { reader.splice.getBool(field.splice)}.tree
      else if (tpe =:= typeOf[Array[Byte]]) reify { reader.splice.getBytes(field.splice)}.tree
      else if (tpe =:= typeOf[Date])        reify { reader.splice.getDate(field.splice) }.tree
      else if (tpe =:= typeOf[String])      { rparseString(field, reader).tree }
      else if (tpe =:= typeOf[Char])        reify {
        val str = rparseString(field, reader).splice
        if (str.length != 1)
          throw new IllegalStateException(s"String $str is too long to be converted to Char")
        str.charAt(0)
      }.tree
      else if (tpe =:= typeOf[scala.Symbol]) { rparseSymbol(field, reader).tree }
      else throw new java.lang.NoSuchFieldException(s"Type '$tpe' is not a primitive!")
    }

    def buildPrimitiveOpt(tpe: Type, field: c.Expr[String], reader: c.Expr[ObjectReader]): c.Expr[Option[_]] = {
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
      else if (tpe =:= typeOf[Date])         reify {
        // Todo: Make this more flexible
        reader.splice.optString(field.splice).map(new SimpleDateFormat().parse(_))
      }
      else if (tpe =:= typeOf[scala.Symbol]) reify {
        reader.splice.optString(field.splice).map(Symbol(_))
      }
      else throw new java.lang.NoSuchFieldException(s"Type '$tpe' is not a primitive!")
    }

    // Builds a class and sets its fields if they are detected
    def buildObject(tpe: Type, reader: c.Expr[ObjectReader]): Tree = {
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

      def buildObject(): Tree = {
        val ctor = tpe.member(nme.CONSTRUCTOR).asMethod
        if (ctor.alternatives.length > 1)
          c.error(c.enclosingPosition, s"Object of type ${tpe} has multiple constructors and cannot be deserialized")

        ctor.paramss.map(_.zipWithIndex.map {
          case (pSym, index) =>
            // Change out the types if it has type parameters
            val pTpe = pSym.typeSignature.substituteTypes(sym.asClass.typeParams, tpeArgs)
            val fieldName = c.literal(pSym.name.decoded)

            // If param has defaults, try to find the val in map, or call
            // default evaluation from its companion object
            if (pSym.asTerm.isParamWithDefault && isPrimitive(pTpe) && sym.companionSymbol.isTerm) {
              reify {
                buildPrimitiveOpt(pTpe, fieldName, orExpr).splice
                  .getOrElse(c.Expr(Select(Ident(sym.companionSymbol), newTermName(
                  "$lessinit$greater$default$" + (index+1).toString))
                ).splice)
              }.tree
            } else if (pSym.asTerm.isParamWithDefault && sym.companionSymbol.isTerm) {
              reify {
                try {
                  c.Expr(buildField(pTpe, fieldName, orExpr)).splice // splice in another obj tree
                } catch {
                  case e: MappingException =>
                    // Need to use the original symbol.companionObj to get defaults
                    // Would be better to find the generated TermNames if possible
                    c.Expr(Select(Ident(sym.companionSymbol), newTermName(
                      "$lessinit$greater$default$" + (index+1).toString))
                    ).splice
                }
              }.tree
            } else buildField(pTpe, fieldName, orExpr)
          }).foldLeft[Tree](Select(New(newObjTypeTree), nme.CONSTRUCTOR)){(a ,b) => Apply(a, b) }
      }


      val newObjTerm = newTermName(c.fresh("newObj$"))
      val newObjTree = ValDef(Modifiers(), newObjTerm, newObjTypeTree,
        buildObject()
      )

      Block(orTree::newObjTree::Nil, Ident(newObjTerm))
    }

    val tpe = weakTypeOf[U]

    val expr = c.Expr[U](buildObject(tpe, reader))

    //println(expr)  // Debug
    expr
  }
}
