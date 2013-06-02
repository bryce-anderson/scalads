package macroimpls

import language.experimental.macros
import scala.reflect.macros.Context
import util.{EntityBacker, Query, QueryIterator}

import com.google.appengine.api.datastore.Query._
import com.google.appengine.api.datastore.{Query => GQuery, DatastoreService}
import macro_readers.GAEObjectReader
import macroimpls.macrohelpers.MacroHelpers

/**
 * @author Bryce Anderson
 *         Created on 5/30/13
 */



object QueryMacros {

  def ObjApplyImpl[U: c.WeakTypeTag](c: Context)(datastore: c.Expr[DatastoreService]): c.Expr[Query[U]] = {
    val helpers = new MacroHelpers[c.type](c)
    val nameExpr = helpers.classNameExpr(c.universe.weakTypeOf[U])
    c.universe.reify (new Query[U](datastore.splice, new GQuery(nameExpr.splice)))
  }

  def findName(c: Context)(tree: c.Tree, name: c.Name): String = {
    import c.universe._
    def findName(tree: Tree, stack: List[String]): String = tree match {
      case Select(Ident(inName), otherTree) if inName == name =>  // Finished
        stack.foldLeft(otherTree.decoded){(a, b) => a + "." + b }
      case Select(Ident(inName), otherTree) if inName != name => throw new MatchError(tree)
      case Select(inner, outer) => findName(inner, outer.encoded::stack)
      //case e => println(s"Failed on tree: ${showRaw(e)}"); sys.error("")   // TODO: debug
    }
    findName(tree, Nil)
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

  def getIteratorImpl[U: c.WeakTypeTag](c: Context { type PrefixType = Query[U]}): c.Expr[QueryIterator[U with EntityBacker[U]]] = {
    import c.universe._

    val deserializeExpr = Deserializer.deserializeImpl[U](c)(c.Expr[GAEObjectReader](Ident(newTermName("reader"))))

    val result = reify {
      new QueryIterator[U with EntityBacker[U]](c.prefix.splice.runQuery, {
        entity =>
          val reader = GAEObjectReader(entity)
          deserializeExpr.splice
      })
    }
    result
  }


  def filterImpl[U: c.WeakTypeTag](c: Context {type PrefixType = Query[U]})(f: c.Expr[U => Boolean]): c.Expr[Query[U]] = {
    import c.universe._

    val Function(List(ValDef(_, name, _, _)), body) = f.tree

    body match { // Make things simple
      case Block(_, _) => c.error(c.enclosingPosition, s"Filter must have single statement filter. Received: $body")
      case _ =>
    }

    def makeFilter(operation: Name, name: String, value: Tree): c.Expr[FilterPredicate] = {
      val op: c.Expr[FilterOperator] = operation.encoded match {
        case "$less" =>       reify(FilterOperator.LESS_THAN)
        case "$greater" =>     reify(FilterOperator.GREATER_THAN)
        case "$less$eq" =>    reify(FilterOperator.LESS_THAN_OR_EQUAL)
        case "$greater$eq" => reify(FilterOperator.GREATER_THAN_OR_EQUAL)
        case "$eq$eq" =>      reify(FilterOperator.EQUAL)
        case "$bang$eq" =>    reify(FilterOperator.NOT_EQUAL)
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
