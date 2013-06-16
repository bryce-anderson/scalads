package scalads.macroimpls

import language.experimental.macros
import scala.reflect.macros.Context

import scalads.core._
import scalads.readers.ObjectReader
import scalads.core.Filter
import scala.collection.mutable.ListBuffer
import scalads.macroimpls.macrohelpers.{MacroHelpers, QueryMacroHelpers}


object QueryMacros {

  def project[U: c.WeakTypeTag, R, E](c: Context { type PrefixType = Query[U, E]})(f: c.Expr[U => R]): c.Expr[QueryIterator[R, E]] = {
    val helpers = new macrohelpers.MacroHelpers[c.type](c)
    import helpers.mkList

    val queryHelpers = new QueryMacroHelpers[c.type](c)
    import queryHelpers.{findNameOption, getReaderAndName}

    val deserializers = new DeserializerBase[c.type](c)
    import deserializers.buildField

    import c.universe._

    val Function(List(ValDef(_, name, _, _)), body) = f.tree

    var projections = new ListBuffer[(List[String], Type)]

    val splicer = new Transformer {
      override def transform(tree: Tree): Tree = {
        findNameOption(tree, name, weakTypeOf[U]) match {
          case Some((stack, tpe)) =>
            // Add the projections
            if (helpers.isPrimitive(tpe)) { projections += ((stack, tpe)) }
            else queryHelpers.getPathStacks(tpe).foreach{i => projections += ((stack:::i._1, i._2)) }

          // Build the expression tree
            val (readerExpr, key) = getReaderAndName(c.Expr[ObjectReader](Ident(newTermName("reader"))), stack)
            val nameExpr = c.literal(key)
            buildField(tpe, nameExpr, readerExpr)

          case None => super.transform(tree)
        }
      }
    }

    val bodyTree = c.Expr[R](c.resetLocalAttrs(splicer.transform(body)))

    val projExprs = projections.result.map{ case (p, t) =>
      val projsExpr = mkList[String](p.map(c.literal(_)))
      val clazzExpr = c.Expr[Class[_]](Literal(Constant(t)))
      reify(Projection(projsExpr.splice, clazzExpr.splice))
    }

    val projLstExpr = mkList[Projection](projExprs)

    val result = reify {
      c.prefix.splice.projectAndMap(projLstExpr.splice,  (_, reader) => bodyTree.splice)
    }
    //println(s"Projection: $result")
    result
  }

  def sortImplGeneric[U: c.WeakTypeTag, Repr <: Query[U, _]](c: Context {type PrefixType = Repr})
                   (f: c.Expr[U => Any], dir: c.Expr[SortDirection]): c.Expr[Repr#Repr] = {
    val queryHelpers = new QueryMacroHelpers[c.type](c)
    val helpers = new MacroHelpers[c.type](c)
    import queryHelpers.findPathStack
    import helpers.mkList

    import c.universe._

    val Function(List(ValDef(_, name, _, _)), body) = f.tree
    val clazzExpr = c.Expr[Class[_]](Literal(Constant(body.tpe)))
    val pathStack = mkList[String](findPathStack(body, name).map(c.literal(_)))
    val projExpr = reify(Projection(pathStack.splice, clazzExpr.splice))

    val result = reify(c.prefix.splice.sortBy(projExpr.splice, dir.splice))
    result
  }

  def sortImplAsc[U: c.WeakTypeTag, Repr <: Query[U, _]](c: Context {type PrefixType = Repr})(f: c.Expr[U => Any]): c.Expr[Repr#Repr] = {
    import c.universe._
    sortImplGeneric[U, Repr](c)(f, reify(SortDirection.ASC))
  }

  def sortImplDesc[U: c.WeakTypeTag, Repr <: Query[U, _]](c: Context {type PrefixType = Repr})(f: c.Expr[U => Any]): c.Expr[Repr#Repr] = {
    import c.universe._
    sortImplGeneric[U, Repr](c)(f, reify(SortDirection.DSC))
  }

  def filterImpl[U: c.WeakTypeTag, Repr <: Query[U, _]](c: Context {type PrefixType = Repr})(f: c.Expr[U => Boolean]): c.Expr[Repr#Repr] = {
    val helpers = new macrohelpers.MacroHelpers[c.type](c)
    import helpers.mkList

    val queryHelpers = new QueryMacroHelpers[c.type](c)
    import queryHelpers.findPathStack

    import c.universe._

    val Function(List(ValDef(_, name, _, _)), body) = f.tree

    body match { // Make things simple
      case Block(_, _) => c.error(c.enclosingPosition, s"Filter must have single statement filter. Received: $body")
      case _ =>
    }

    def makeFilter(operation: Name, path: List[String], value: Tree): c.Expr[Filter] = {
      val op: c.Expr[Operation] = operation.decoded match {
        case "<"  =>    reify(Operation.LT)
        case ">"  =>    reify(Operation.GT)
        case "<=" =>    reify(Operation.LE)
        case ">=" =>    reify(Operation.GE)
        case "==" =>    reify(Operation.EQ)
        case "!=" =>    reify(Operation.NE)
        case _ => sys.error("Failed to find operator")
      }
      val nameExpr = mkList[String](path.map(c.literal(_)))
      val classExpr = c.Expr[Class[_]](Literal(Constant(value.tpe)))
      reify (SingleFilter(Projection(nameExpr.splice, classExpr.splice), op.splice, c.Expr[Any](value).splice))
    }

    def makeComposite(f1: c.Expr[Filter], f2: c.Expr[Filter], operation: Name): c.Expr[CompositeFilter] = operation.decoded match {
      case "||" =>       reify(CompositeFilter(f1.splice, f2.splice, JoinOperation.OR))
      case "&&" =>       reify(CompositeFilter(f1.splice, f2.splice, JoinOperation.AND))
      case _ => sys.error("Failed to find operator")
    }

    // TODO: Can this be cleaned up substantially? It seems a bit clunky
    def decompose(tree: Tree): c.Expr[Filter] = try { tree match {
      case Apply(Select(Apply(Select(Select(This(scalaTypeName), pName), augName), List(tree1)),operator),tree2) if (
        scalaTypeName == newTypeName("scala") &&
          pName == newTermName("Predef") &&
          augName == newTermName("augmentString")
        ) => decompose(Apply(Select(tree1, operator), tree2)) // Repackage

      case Apply(Select(t1 @ Apply(_ , _), operator), List(t2)) =>
        makeComposite(decompose(t1), decompose(t2), operator)

      case Apply(Select(firstTree, operation), List(secondTree)) =>
        try { makeFilter(operation, findPathStack(firstTree, name), secondTree) } catch {
          // Try it backwards in case they did something like '4 == foo.bar'
          case e: MatchError => makeFilter(operation, findPathStack(secondTree, name), firstTree)
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
