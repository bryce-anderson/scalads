package macroimpls

import language.experimental.macros
import scala.reflect.macros.Context
import util.{QueryIterator, Datastore, Query}
import scala.util.control.Exception.catching

import com.google.appengine.api.datastore.Query._
import com.google.appengine.api.datastore.{Query => GQuery, PropertyProjection, Projection, Entity, DatastoreService}
import macro_readers.GAEObjectReader
import macroimpls.macrohelpers.MacroHelpers

/**
 * @author Bryce Anderson
 *         Created on 5/30/13
 */



object QueryMacros {

  def findName(c: Context)(tree: c.Tree, name: c.Name) = findPath(c)(tree, name).mkString(".")

  def findPath(c: Context)(tree: c.Tree, name: c.Name): List[String] = {
    import c.universe._
    def findName(tree: Tree, stack: List[String]): List[String] = tree match {
      case Select(Ident(inName), otherTree) if inName == name => otherTree.decoded::stack // Finished
      case Select(Ident(inName), otherTree) if inName != name => throw new MatchError(tree)
      case Select(inner, outer) => findName(inner, outer.encoded::stack)
      //case e => println(s"Failed on tree: ${showRaw(e)}"); sys.error("")   // TODO: debug
    }
    findName(tree, Nil)
  }

  def project[U: c.WeakTypeTag, R](c: Context { type PrefixType = Query[U]})(f: c.Expr[U => R]): c.Expr[QueryIterator[R]] = {
    import c.universe._

    val Function(List(ValDef(_, name, _, _)), body) = f.tree
    val tpe = weakTypeOf[U]

    body match { // Make things simple
      case Block(_, _) => c.error(c.enclosingPosition, s"Filter must have single statement filter. Received: $body")
      case _ =>
    }

    val Apply(ctorTree, args) = body

    val fieldTypes: Map[String, Type] = args.map{tree => catching(classOf[MatchError]).opt(findPath(c)(tree, name))}
      .collect{ case Some(str) => str }
      .map { stack =>
        stack.foldLeft(tpe){ (t, n) => t.member(newTermName(n)).typeSignature } match {
          case tpe if tpe.typeSymbol.name.decoded == "Int" =>    (stack.mkString("."), typeOf[Int])
          case tpe if tpe.typeSymbol.name.decoded == "Long" =>   (stack.mkString("."), typeOf[Long]  )
          case tpe if tpe.typeSymbol.name.decoded == "Double" => (stack.mkString("."), typeOf[Double])
          case tpe if tpe.typeSymbol.name.decoded == "Float" =>  (stack.mkString("."), typeOf[Float]   )
          case tpe if tpe.typeSymbol.name.decoded == "String" => (stack.mkString("."), typeOf[String]  )
          case tpe if tpe.typeSymbol.name.decoded == "Date" =>   (stack.mkString("."), typeOf[java.util.Date])
        }
      }.toMap

    def setType(tpe: Type): c.Expr[Class[_]] = tpe match {
      case tpe if tpe =:= typeOf[Int]    || tpe=:= typeOf[java.lang.Integer] =>  reify(classOf[java.lang.Integer])
      case tpe if tpe =:= typeOf[Long]   || tpe=:= typeOf[java.lang.Long] =>  reify(classOf[java.lang.Long])
      case tpe if tpe =:= typeOf[Double] || tpe=:= typeOf[java.lang.Double] =>  reify(classOf[java.lang.Double])
      case tpe if tpe =:= typeOf[Float]  || tpe=:= typeOf[java.lang.Float] =>  reify(classOf[java.lang.Float])
      case tpe if tpe =:= typeOf[String] =>  reify(classOf[java.lang.String])
      case tpe if tpe =:= typeOf[java.util.Date] =>  reify(classOf[java.util.Date])
    }

    val qExpr = {
      var projections: List[String] = Nil
      args.map{tree =>
        try{ Right(findName(c)(tree, name))
        } catch {
          case _: MatchError => Left(tree)
        }
      }.foldLeft(c.prefix){ (q, either) => either match {
        case Right(str) if !projections.contains(str) =>
          projections = str::projections
          reify{q.splice.setProjection(new PropertyProjection(c.literal(str).splice, setType(fieldTypes(str)).splice))}
        case _ => q
        }
      }
    }

    def pathReader(reader: c.Expr[GAEObjectReader], stack: List[String]): (c.Expr[GAEObjectReader], String) = stack match {
      case str::Nil => (reader, str)
      case h::t =>  val name = c.literal(h); pathReader(reify(reader.splice.getObjectReader(name.splice)), t)
    }

    val entityExtractors: List[Tree] = args.map{tree => try {Right(findPath(c)(tree, name))} catch {case m: MatchError => Left(tree)} }
      .map{ treeOrName =>
      treeOrName.fold( identity, { nameStack =>
        val (readerExpr, key) = pathReader(c.Expr[GAEObjectReader](Ident(newTermName("reader"))), nameStack)
        val nameExpr = c.literal(key)
        fieldTypes(nameStack.mkString(".")) match {
          case tpe if tpe =:= typeOf[Int] || tpe=:= typeOf[java.lang.Integer] => reify{readerExpr.splice.getInt(nameExpr.splice)}.tree
          case tpe if tpe =:= typeOf[Long] || tpe=:= typeOf[java.lang.Long] => reify{readerExpr.splice.getLong(nameExpr.splice)}.tree
          case tpe if tpe =:= typeOf[Float] || tpe=:= typeOf[java.lang.Float] => reify{readerExpr.splice.getFloat(nameExpr.splice)}.tree
          case tpe if tpe =:= typeOf[Double] || tpe=:= typeOf[java.lang.Double] => reify{readerExpr.splice.getDouble(nameExpr.splice)}.tree
          case tpe if tpe =:= typeOf[String] => reify{readerExpr.splice.getString(nameExpr.splice)}.tree
          case tpe if tpe =:= typeOf[java.util.Date] => reify{readerExpr.splice.getDate(nameExpr.splice)}.tree
        }
      })
    }

    val entityExpr = c.Expr[Entity](Ident(newTermName("entity")))
    val Block(readerTree::Nil, _) = reify{val reader = new GAEObjectReader(entityExpr.splice, "")}.tree

    val applyExpr = c.Expr[R](Block(readerTree::Nil, Apply(ctorTree, entityExtractors)))

    val result = reify(qExpr.splice.mapIterator{entity => applyExpr.splice })
    println(result)
    result
  }

  def sortImplGeneric[U: c.WeakTypeTag](c: Context {type PrefixType = Query[U]})(f: c.Expr[U => Any], dir: c.Expr[SortDirection]): c.Expr[Query[U]] = {
    import c.universe._

    val Function(List(ValDef(_, name, _, _)), body) = f.tree
    val nameStr = c.Expr[String](Literal(Constant(findName(c)(body, name))))

    val result = reify{
      c.prefix.splice.sortBy(nameStr.splice, dir.splice)
    }
    result
  }

  def sortImplAsc[U: c.WeakTypeTag](c: Context {type PrefixType = Query[U]})(f: c.Expr[U => Any]): c.Expr[Query[U]] = {
    import c.universe._
    sortImplGeneric(c)(f, reify(SortDirection.ASCENDING))
  }

  def sortImplDesc[U: c.WeakTypeTag](c: Context {type PrefixType = Query[U]})(f: c.Expr[U => Any]): c.Expr[Query[U]] = {
    import c.universe._
    sortImplGeneric(c)(f, reify(SortDirection.DESCENDING))
  }

  def filterImpl[U: c.WeakTypeTag](c: Context {type PrefixType = Query[U]})(f: c.Expr[U => Boolean]): c.Expr[Query[U]] = {
    import c.universe._

    val Function(List(ValDef(_, name, _, _)), body) = f.tree

    body match { // Make things simple
      case Block(_, _) => c.error(c.enclosingPosition, s"Filter must have single statement filter. Received: $body")
      case _ =>
    }

    def makeFilter(operation: Name, name: String, value: Tree): c.Expr[FilterPredicate] = {
      val op: c.Expr[FilterOperator] = operation.decoded match {
        case "<"  =>    reify(FilterOperator.LESS_THAN)
        case ">"  =>    reify(FilterOperator.GREATER_THAN)
        case "<=" =>    reify(FilterOperator.LESS_THAN_OR_EQUAL)
        case ">=" =>    reify(FilterOperator.GREATER_THAN_OR_EQUAL)
        case "==" =>    reify(FilterOperator.EQUAL)
        case "!=" =>    reify(FilterOperator.NOT_EQUAL)
        case _ => sys.error("Failed to find operator")
      }
      val nameExpr = c.Expr[String](Literal(Constant(name)))
      reify (new FilterPredicate(nameExpr.splice, op.splice, c.Expr[Any](value).splice))
    }

    def makeComposite(f1: c.Expr[Filter], f2: c.Expr[Filter], operation: Name): c.Expr[CompositeFilter] = operation.encoded match {
      case "$bar$bar" =>       reify(CompositeFilterOperator.or(f1.splice, f2.splice))
      case "$amp$amp" =>       reify(CompositeFilterOperator.and(f1.splice, f2.splice))
      case _ => sys.error("Failed to find operator")
    }

    def decompose(tree: Tree): c.Expr[Filter] = try { tree match {
      case Apply(Select(Apply(Select(Select(This(scalaTypeName), pName), augName), List(tree1)),operator),tree2) if (
        scalaTypeName == newTypeName("scala") &&
          pName == newTermName("Predef") &&
          augName == newTermName("augmentString")
        ) => decompose(Apply(Select(tree1, operator), tree2)) // Repackage

      case Apply(Select(t1 @ Apply(_ , _), operator), List(t2)) =>
        //println("making composite")
        makeComposite(decompose(t1), decompose(t2), operator)

      case Apply(Select(firstTree, operation), List(secondTree)) =>
        try { makeFilter(operation, findName(c)(firstTree, name), secondTree) } catch {
          // Try it backwards
          case e: MatchError => makeFilter(operation, findName(c)(secondTree, name), firstTree)
        }
    } } catch {
      case e: MatchError => c.error(c.enclosingPosition, s"Cannot decompose operation the operation: $tree"); sys.error("")
    }

    val filter = decompose(body)
    //println(s"------------------Body:\n${showRaw(body)}")
    //println(s"----------------- Decomposed:\n${filter.tree}")

    reify{c.prefix.splice.setFilter(filter.splice)}
  }

}
