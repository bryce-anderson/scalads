package macroimpls

import language.experimental.macros
import scala.reflect.macros.Context
import scala.Some
import macro_readers.{JsonReader, JsonObjectReader, JsonArrayIterator}
import macroimpls.macrohelpers.MacroHelpers
import java.text.SimpleDateFormat
import exceptions.MappingException


object Deserializer {

  import java.util.Date

  def deserialize[U: c.WeakTypeTag](c: Context)(reader: c.Expr[JsonReader]): c.Expr[U] = {

    val helpers = new MacroHelpers[c.type](c)
    import helpers.{isPrimitive,LIT,typeArgumentTree}
    import c.universe._

    def rparseDate(field: c.Expr[String], reader: c.Expr[JsonObjectReader])  = reify {
      new SimpleDateFormat().parse(rparseString(field, reader).splice)
    }

    def rparseString(field: c.Expr[String], reader: c.Expr[JsonObjectReader]) = reify {
      reader.splice.getString(field.splice)
    }

    def rparseSymbol(field: c.Expr[String], reader: c.Expr[JsonObjectReader]) = reify {
      Symbol(rparseString(field, reader).splice)
    }

    def rparseOption(tpe:Type, field: c.Expr[String], reader: c.Expr[JsonObjectReader]):Tree = {
      val TypeRef(_, _, List(argTpe)) = tpe
      reify{
        try{
          Some(c.Expr(buildField(argTpe, field, reader)).splice)
        } catch {
          case _: Throwable => None
        }
      }.tree
    }

    def buildMap(tpe:Type, reader: c.Expr[JsonObjectReader]): c.Tree   = {
      val TypeRef(_, _, keyTpe::valTpe::Nil) = tpe
      // Capable of parsing maps that contain primitives as keys, not only strings
      val kExpr = c.Expr[String](Ident("k"))
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

    def buildList(tpe: Type, reader: c.Expr[JsonArrayIterator]): Tree = {
      val TypeRef(_, _, List(argTpe)) = tpe

      val builderExpr = c.Expr[collection.mutable.Builder[Any, List[Any]]](TypeApply(Select(
        Ident("List"), newTermName("newBuilder")), List(TypeTree(argTpe))))

      val itNme = c.fresh("jsonIterator$")
      val itExpr = c.Expr[JsonArrayIterator](Ident(itNme))
      val itTree = ValDef(
        Modifiers(),
        newTermName(itNme),
        TypeTree(typeOf[JsonArrayIterator]),
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

    // builds the cells of a list
    def buildCell(tpe: Type, reader: c.Expr[JsonArrayIterator]): Tree = {
      if      (tpe =:= typeOf[Int])           reify { reader.splice.nextInt }.tree
      else if (tpe =:= typeOf[Long])          reify { reader.splice.nextLong }.tree
      else if (tpe =:= typeOf[Float])         reify { reader.splice.nextFloat }.tree
      else if (tpe =:= typeOf[Double])        reify { reader.splice.nextDouble }.tree
      else if (tpe =:= typeOf[String])        reify { reader.splice.nextString }.tree
      else if (typeOf[List[_]] <:< tpe.erasure) buildList(tpe, reify{reader.splice.nextArrayReader})
      else if (typeOf[Map[_, _]] <:< tpe.erasure) {
        val orNme = c.fresh("jsonReader$")
        val orExpr = c.Expr[JsonObjectReader](Ident(orNme))
        val orTree = ValDef(
          Modifiers(),
          newTermName(orNme),
          TypeTree(typeOf[JsonObjectReader]),
          reify{reader.splice.nextObjectReader}.tree
        )
        Block(orTree, buildMap(tpe, orExpr))
      }
      else buildObject(tpe, reify{reader.splice.nextObjectReader})
    }

    // builds the different fields of an Object or Map
    def buildField(tpe: Type, fieldName: c.Expr[String], reader: c.Expr[JsonObjectReader]): Tree = {
      if (isPrimitive(tpe)) buildPrimitive(tpe, fieldName, reader)
      // The privileged types
      else if (tpe.erasure <:< typeOf[Option[_]]) {
        rparseOption(tpe, fieldName, reader)
      }
      else if (typeOf[Map[_, _]] <:< tpe.erasure) {
        val orNme = c.fresh("jsonReader$")
        val orExpr = c.Expr[JsonObjectReader](Ident(orNme))
        val orTree = ValDef(
          Modifiers(),
          newTermName(orNme),
          TypeTree(typeOf[JsonObjectReader]),
          reader.tree
        )
        Block(orTree, buildMap(tpe, orExpr))
      }
      else if (typeOf[List[_]] <:< tpe.erasure) {
        buildList(tpe, reify{reader.splice.getArrayReader(fieldName.splice)})
      }
      else  buildObject(tpe, reify{ reader.splice.getObjectReader(fieldName.splice)})
    }

    def buildPrimitive(tpe: Type, field: c.Expr[String], reader: c.Expr[JsonObjectReader]) = {
      if      (tpe =:= typeOf[Int])         reify {reader.splice.getInt(field.splice)    }.tree
        // TODO: type Byte and Char
      else if (tpe =:= typeOf[Short])       reify {reader.splice.getInt(field.splice).asInstanceOf[Short]}.tree
      else if (tpe =:= typeOf[Byte])        reify {reader.splice.getInt(field.splice).asInstanceOf[Byte] }.tree
      else if (tpe =:= typeOf[Long])        reify { reader.splice.getLong(field.splice)  }.tree
      else if (tpe =:= typeOf[Float])       reify { reader.splice.getFloat(field.splice) }.tree
      else if (tpe =:= typeOf[Double])      reify { reader.splice.getDouble(field.splice)}.tree
      else if (tpe =:= typeOf[Boolean])      reify { reader.splice.getBool(field.splice)}.tree
      else if (tpe =:= typeOf[String])      { rparseString(field, reader).tree }
      else if (tpe =:= typeOf[Char])        reify {
        val str = rparseString(field, reader).splice
        if (str.length != 1)
          throw new IllegalStateException(s"String $str is too long to be converted to Char")
        str.charAt(0)
      }.tree
      else if (tpe =:= typeOf[Date])         { rparseDate(field, reader).tree   }
      else if (tpe =:= typeOf[scala.Symbol]) { rparseSymbol(field, reader).tree }
      else throw new java.lang.NoSuchFieldException(s"Type '$tpe' is not a primitive!")
    }

    def buildPrimitiveOpt(tpe: Type, field: c.Expr[String], reader: c.Expr[JsonObjectReader]): c.Expr[Option[_]] = {
      if      (tpe =:= typeOf[Int])         reify {reader.splice.optInt(field.splice)     }
      else if (tpe =:= typeOf[Short])       reify {reader.splice.optInt(field.splice).map(_.asInstanceOf[Short])}
      else if (tpe =:= typeOf[Byte])       reify {reader.splice.optInt(field.splice).map(_.asInstanceOf[Byte])}
      else if (tpe =:= typeOf[Long])        reify { reader.splice.optLong(field.splice)   }
      else if (tpe =:= typeOf[Float])       reify { reader.splice.optFloat(field.splice)  }
      else if (tpe =:= typeOf[Double])      reify { reader.splice.optDouble(field.splice) }
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
    def buildObject(tpe: Type, reader: c.Expr[JsonObjectReader]): Tree = {
      // Find some info on our object type
      val TypeRef(_, sym: Symbol, tpeArgs: List[Type]) = tpe
      val newObjTypeTree = typeArgumentTree(tpe)

      // Make the object reader tree bits
      val orNme = c.fresh("jsonReader$")
      val orExpr = c.Expr[JsonObjectReader](Ident(orNme))
      val orTree = ValDef(
        Modifiers(),
        newTermName(orNme),
        TypeTree(typeOf[JsonObjectReader]),
        reader.tree
      )

      // Builds the if/else tree for checking constructor params and returning a new object
      def pickConstructorTree(argNames: c.Expr[Set[String]]): Tree = {
        // Makes expressions for determining of they list is satisfied by the reader
        def ctorCheckingExpr(ctors: List[List[Symbol]]): c.Expr[Boolean] = {
          def isRequired(item: Symbol) = {
            val sym = item.asTerm
            !(sym.isParamWithDefault || sym.typeSignature <:< typeOf[Option[_]])
          }

          val expr = c.Expr[Set[String]](Apply(Select(Ident("Set"), newTermName("apply")),
            ctors.flatten
              .filter(isRequired(_))
              .map(sym => Literal(Constant(sym.name.decoded)))
          ))

          reify(expr.splice.subsetOf(argNames.splice))
        }

        def ifElseTreeBuilder(ctorSets: List[(c.Expr[Boolean], List[List[Symbol]])]): Tree = ctorSets match {
          case h::Nil => buildObjFromParams(h._2)
          case h::t => If(h._1.tree, buildObjFromParams(h._2), ifElseTreeBuilder(t))
        }

        val ctors: List[MethodSymbol] = tpe.member(nme.CONSTRUCTOR)
          .asTerm.alternatives   // List of constructors
          .map(_.asMethod)       // method symbols
          .sortBy(-_.paramss.flatten.size)
        val ifExprsAndParams = ctors.map(ctor => ctorCheckingExpr(ctor.paramss)).zip(ctors.map(_.asMethod.paramss))

        ifElseTreeBuilder(ifExprsAndParams)
      }

      def buildObjFromParams(ctorParams: List[List[Symbol]]): Tree =
        New(newObjTypeTree, ctorParams.map(_.zipWithIndex.map {
          case (pSym, index) =>
            // Change out the types if it has type parameters
            val pTpe = pSym.typeSignature.substituteTypes(sym.asClass.typeParams, tpeArgs)
            val fieldName = LIT(pSym.name.decoded)

            // If param has defaults, try to find the val in map, or call
            // default evaluation from its companion object
            // TODO: is the sym.companionSymbol.isTerm the best way to check for NoSymbol?
            // TODO: is there a way to get teh default values for the overloaded constructors?
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
          })
        )

      // Sets fields after the instance is has been created
      def optionalParams(pTpe: Type, varName: String, exprMaker: Tree => c.Expr[_]): Tree = {
        val compName = LIT(varName)
        // Use option if primitive, should be faster than exceptions.
        if(isPrimitive(pTpe)) reify {
          buildPrimitiveOpt(pTpe, compName, orExpr).splice match {
            case Some(x) => exprMaker(Ident("x")).splice
            case None =>
          }
        }.tree
        else if(typeOf[List[_]] <:< pTpe.erasure) reify {
          reader.splice.optArrayReader(compName.splice) match {
            case Some(x) => exprMaker(buildList(pTpe,c.Expr[JsonArrayIterator](Ident("x")))).splice
            case None =>
          }
        }.tree
        else if(typeOf[Map[_, _]] <:< pTpe.erasure) reify {
          reader.splice.optObjectReader(compName.splice) match {
            case Some(x) => exprMaker(buildMap(pTpe,c.Expr[JsonObjectReader](Ident("x")))).splice
            case None =>
          }
        }.tree
        else reify {
          reader.splice.optObjectReader(compName.splice) match {
            case Some(x) => exprMaker(buildObject(pTpe,c.Expr[JsonObjectReader](Ident("x")))).splice
            case None =>
          }
        }.tree
      }

      val newObjTerm = newTermName(c.fresh("newObj$"))
      val newObjTree = ValDef(Modifiers(), newObjTerm, newObjTypeTree,
        pickConstructorTree(reify(orExpr.splice.getKeys))
      )

//      val setVarsBlocks =
//        getNonConstructorVars(tpe).map{ pSym =>
//          val varName = pSym.name.toTermName.toString.trim
//          val tpe = pSym.typeSignature.substituteTypes(sym.asClass.typeParams, tpeArgs)
//          optionalParams(tpe, varName,
//            tree => c.Expr(Assign(Select(Ident(newObjTerm), newTermName(varName)), tree))
//          )
//        }
//
//      val setSetterBlocks =
//        getJavaStyleSetters(tpe).map { pSym =>  // MethodSymbol
//          val origName = pSym.name.decoded.substring(3)
//          val name = origName.charAt(0).toLower + origName.substring(1)
//          val paramType = {
//            val tpe = pSym.asMethod.paramss(0)(0)
//            tpe.typeSignature.substituteTypes(sym.asClass.typeParams, tpeArgs)
//          }
//          optionalParams(paramType, name,
//            tree =>  c.Expr(Apply(Select(Ident(newObjTerm), pSym.name), tree::Nil))
//          )
//      }

      //Block(orTree::newObjTree::setVarsBlocks:::setSetterBlocks, Ident(newObjTerm))
      Block(orTree::newObjTree::Nil, Ident(newObjTerm))
    }
    
    val tpe = weakTypeOf[U]

    // The three fundamental types that can be deserialized
    val expr = if (typeOf[Map[_, _]] <:< tpe.erasure) {
      val i = c.Expr[U](buildMap(tpe, c.Expr[JsonObjectReader](Ident("r"))))
      reify {
        reader.splice match {
          case r: JsonObjectReader => i.splice
          case e => throw new IllegalStateException(s"Need reader type Object to read object fields. Found: ${e.getClass.toString}")
        }
      }
    }
    else if (typeOf[List[_]] <:< tpe.erasure) {
      val i = c.Expr[U](buildList(tpe, c.Expr[JsonArrayIterator](Ident("r"))))
      reify {
        reader.splice match {
          case r: JsonArrayIterator => i.splice
          case e => throw new IllegalStateException(s"Need reader type Array to read object fields. Found: ${e.getClass.toString}")
        }
      }
    } else {
      val i = c.Expr[U](buildObject(tpe, c.Expr[JsonObjectReader](Ident("r"))))
      reify {
        reader.splice match {
          case r: JsonObjectReader => i.splice
          case e => throw new IllegalStateException(s"Need reader type Object to read object fields. Found: ${e.getClass.toString}")
        }
      }
    }
    //println(expr)  // Debug
    expr
  }
}
