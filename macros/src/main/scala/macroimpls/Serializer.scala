package macroimpls

import language.experimental.macros
import scala.reflect.macros.Context
import java.util.Date
import util.Utils._

import macrohelpers._

import writers.Writer


// Intended to be the serialization side of the class builder
trait Serializer extends MacroHelpers {

  import c.universe._
  lazy val primitiveTypes =
    (typeOf[Int], (writer: c.Expr[Writer[_]], t: Tree) => reify{writer.splice.int(c.Expr[Int](t).splice)})::
      (typeOf[String], (writer: c.Expr[Writer[_]], t: Tree) => reify{writer.splice.string(c.Expr[String](t).splice)})::
      (typeOf[Float], (writer: c.Expr[Writer[_]], t: Tree) => reify{writer.splice.float(c.Expr[Float](t).splice)})::
      (typeOf[Double], (writer: c.Expr[Writer[_]], t: Tree) => reify{writer.splice.double(c.Expr[Double](t).splice)})::
      (typeOf[Boolean], (writer: c.Expr[Writer[_]], t: Tree) => reify{writer.splice.boolean(c.Expr[Boolean](t).splice)})::
      (typeOf[Long], (writer: c.Expr[Writer[_]], t: Tree) => reify{writer.splice.long(c.Expr[Long](t).splice)})::
      (typeOf[Byte], (writer: c.Expr[Writer[_]], t: Tree) => reify{writer.splice.byte(c.Expr[Byte](t).splice)})::
      (typeOf[BigInt], (writer: c.Expr[Writer[_]], t: Tree) => reify{writer.splice.bigInt(c.Expr[BigInt](t).splice)})::
      (typeOf[Short], (writer: c.Expr[Writer[_]], t: Tree) => reify{writer.splice.short(c.Expr[Short](t).splice)})::
      (typeOf[BigDecimal], (writer: c.Expr[Writer[_]], t: Tree) => reify{writer.splice.bigDecimal(c.Expr[BigDecimal](t).splice)})::
      (typeOf[Date], (writer: c.Expr[Writer[_]], t: Tree) => reify{writer.splice.date(c.Expr[Date](t).splice)})::
      (typeOf[scala.Symbol], (writer: c.Expr[Writer[_]], t: Tree) => reify{writer.splice.string(c.Expr[scala.Symbol](t).splice.name)})::
      Nil

  /* ----------------- Macro Serializer ----------------- */
  def serializeImpl[U: c.WeakTypeTag](obj: c.Expr[U], writer: c.Expr[Writer[_]]): c.Expr[Unit] = {

    def listExpr(tpe: Type, path: Tree): Expr[Unit] = {
      val TypeRef(_, _:Symbol, pTpe::Nil) = tpe
      reify{
        writer.splice.startArray()
        c.Expr[scala.collection.Seq[Any]](path).splice.foreach { i =>
          c.Expr(buildTpe(pTpe, Ident(newTermName("i")))).splice
        }
        writer.splice.endArray()
      }
    }

    def mapExpr(tpe: Type, path: Tree): Expr[Unit] = {
      val TypeRef(_, _, keyTpe::valTpe::Nil) = tpe
      if(!isPrimitive(keyTpe)) {
        c.abort(c.enclosingPosition, s"Maps needs to have keys of primitive type! Type: $keyTpe")
      }

      reify{
        writer.splice.startObject()
        c.Expr[scala.collection.GenMap[_, _]](path).splice.foreach { case (k, v) =>
          writer.splice.startField(k.toString)
          c.Expr(buildTpe(valTpe, Ident(newTermName("v")))).splice
        }
        writer.splice.endObject()
      }
    }

    def optionExpr(tpe: Type, path: Tree): Expr[Unit] = {
      val TypeRef(_, _ :Symbol, pTpe::Nil) = tpe
      reify{
        optIdent(c.Expr[Option[_]](path).splice) match {
          case Some(x) => c.Expr[Unit](buildTpe(pTpe, Ident(newTermName("x")))).splice
          case None    => Unit
        }
      }
    }

    def complexObject(oldTpe: Type, path: Tree): c.Tree = {
      val TypeRef(_, sym: Symbol, tpeArgs: List[Type]) = oldTpe

      // Completely flatten this thing out
      val ctorTrees = oldTpe.member(nme.CONSTRUCTOR).asMethod.paramss.flatMap{ _.flatMap{ pSym =>
        if (pSym.isPublic) {
          val tpe = pSym.typeSignature.substituteTypes(sym.asClass.typeParams, tpeArgs)
          val fieldName = pSym.name.decoded.trim
          val fieldPath = Select(path, newTermName(fieldName))
          val startFieldExpr =  reify{writer.splice.startField(LIT(fieldName).splice)}
          startFieldExpr.tree::buildTpe(tpe, fieldPath)::Nil
        } else Nil
      }}
      // Return add all the blocks for each field and pop this obj off the stack
      Block(reify(writer.splice.startObject()).tree::ctorTrees:::
        reify{writer.splice.endObject()}.tree::Nil, EmptyTree)
    }

    def buildTpe(tpe: Type, path: Tree): Tree = primitiveTypes.find(_._1 =:= tpe)
      .map{ case (_, f) => f(writer, path).tree}
      .orElse{if(tpe <:< typeOf[scala.collection.Seq[_]]) Some(listExpr(tpe, path).tree) else None }
      .orElse{if(tpe <:< typeOf[scala.collection.GenMap[_, _]]) {
          Some(mapExpr(tpe, path).tree)
        } else None}
      .orElse{if(tpe <:< typeOf[Option[_]]) Some(optionExpr(tpe, path).tree) else None }
      .getOrElse(complexObject(tpe, path))

    val tpe = weakTypeOf[U]

    // Only basic types are lists maps or objects
    if (isPrimitive(tpe) || tpe =:= typeOf[Option[_]])
      c.abort(c.enclosingPosition,  s"Json4s macros cannot serialize primitive type '$tpe'")

    val tree = if(tpe <:< typeOf[scala.collection.Seq[Any]]) {
      listExpr(tpe, obj.tree).tree
    } else if(tpe <:< typeOf[scala.collection.GenMap[_, _]]) {
      mapExpr(tpe, obj.tree).tree
    } else complexObject(tpe, obj.tree)

    //println(s"------------------ Debug: Generated Code ------------------\n $code")
    c.Expr[Unit](tree)
  }
}
