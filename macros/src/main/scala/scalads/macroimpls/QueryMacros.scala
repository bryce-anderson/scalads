package scalads.macroimpls

import language.experimental.macros
import scala.reflect.macros.Context

import scala.util.control.Exception.catching

import scalads.core._
import scalads.readers.ObjectReader
import scalads.core.Filter

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

  def project[U: c.WeakTypeTag, R, E](c: Context { type PrefixType = Query[U, E]})(f: c.Expr[U => R]): c.Expr[QueryIterator[R, E]] = {
    val helpers = new macrohelpers.MacroHelpers[c.type](c)
    import helpers.mkStringList

    import c.universe._

    val Function(List(ValDef(_, name, _, _)), body) = f.tree
    val tpe = weakTypeOf[U]

    body match { // Make things simple
      case Block(_, _) => c.error(c.enclosingPosition, s"Filter must have single statement filter. Received: $body")
      case _ =>
    }

    def getType(stack: List[String]): Option[Type] = {
      stack.foldLeft[Option[Type]](Some(tpe)){ (t, n) =>
        t.flatMap(_.member(nme.CONSTRUCTOR).asMethod.paramss.flatten.find(_.name.decoded == n).map(_.typeSignature))
      }
    }

    def findNameOption(tree: Tree): Option[(List[String], Type)] = {
      def findNameOption(tree: Tree, stack: List[String]): Option[(List[String], Type)] = tree match {
        case Select(Ident(inName), otherTree) if inName == name =>
          val fullStack = otherTree.decoded::stack
          getType(fullStack).map((fullStack, _))
        case Select(Ident(inName), otherTree) if inName != name => None
        case Select(inner, outer) => findNameOption(inner, outer.encoded::stack)
        case _ => None
        //case e => println(s"Failed on tree: ${showRaw(e)}"); sys.error("")   // TODO: debug
      }
      findNameOption(tree, Nil)
    }

    def pathReader(reader: c.Expr[ObjectReader], stack: List[String]): (c.Expr[ObjectReader], String) = stack match {
      case str::Nil => (reader, str)
      case h::t =>  val name = c.literal(h); pathReader(reify(reader.splice.getObjectReader(name.splice)), t)
    }


    var projections: List[List[String]] = Nil

    val splicer = new Transformer {

      override def transform(tree: Tree): Tree = {
        findNameOption(tree) match {
          case Some((stack, tpe)) =>
            val (readerExpr, key) = pathReader(c.Expr[ObjectReader](Ident(newTermName("reader"))), stack)

            projections = stack::projections

            // Deal with the projection
            val nameExpr = c.literal(key)
            val resultTree = tpe match {
              case tpe if tpe =:= typeOf[Int] || tpe=:= typeOf[java.lang.Integer] => reify{readerExpr.splice.getInt(nameExpr.splice)}.tree
              case tpe if tpe =:= typeOf[Long] || tpe=:= typeOf[java.lang.Long] => reify{readerExpr.splice.getLong(nameExpr.splice)}.tree
              case tpe if tpe =:= typeOf[Float] || tpe=:= typeOf[java.lang.Float] => reify{readerExpr.splice.getFloat(nameExpr.splice)}.tree
              case tpe if tpe =:= typeOf[Double] || tpe=:= typeOf[java.lang.Double] => reify{readerExpr.splice.getDouble(nameExpr.splice)}.tree
              case tpe if tpe =:= typeOf[String] => reify{readerExpr.splice.getString(nameExpr.splice)}.tree
              case tpe if tpe =:= typeOf[java.util.Date] => reify{readerExpr.splice.getDate(nameExpr.splice)}.tree
            }

            //super.transform(tree)
            resultTree


          case None => super.transform(tree)
        }
      }
    }

    val bodyTree = c.Expr[R](c.resetLocalAttrs(splicer.transform(body)))

    val qExpr = projections.foldLeft(c.prefix){ (p, s) =>
      val projExpr = mkStringList(s)
      reify(p.splice.addProjection(Projection(projExpr.splice)))
    }

    val result = reify {
      qExpr.splice.mapIterator( reader => bodyTree.splice)
    }

    println(s"Projection: $result")
    result
  }

  def sortImplGeneric[U: c.WeakTypeTag](c: Context {type PrefixType = Query[U, _]})(f: c.Expr[U => Any], dir: c.Expr[SortDirection]) = {
    import c.universe._

    val Function(List(ValDef(_, name, _, _)), body) = f.tree
    val nameStr = c.Expr[String](Literal(Constant(findName(c)(body, name))))

    val result = reify{
      c.prefix.splice.sortBy(nameStr.splice, dir.splice)
    }
    result
  }

  def sortImplAsc[U: c.WeakTypeTag](c: Context {type PrefixType = Query[U, _]})(f: c.Expr[U => Any]) = {
    import c.universe._
    sortImplGeneric(c)(f, reify(SortDirection.ASC))
  }

  def sortImplDesc[U: c.WeakTypeTag](c: Context {type PrefixType = Query[U, _]})(f: c.Expr[U => Any]) = {
    import c.universe._
    sortImplGeneric(c)(f, reify(SortDirection.DSC))
  }

  def filterImpl[U: c.WeakTypeTag](c: Context {type PrefixType = Query[U, _]})(f: c.Expr[U => Boolean]) = {
    val helpers = new macrohelpers.MacroHelpers[c.type](c)
    import helpers.mkStringList

    import c.universe._

    val Function(List(ValDef(_, name, _, _)), body) = f.tree

    body match { // Make things simple
      case Block(_, _) => c.error(c.enclosingPosition,
                          s"Filter must have single statement filter. Received: $body")
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
      val nameExpr = mkStringList(path)
      reify (SingleFilter(Projection(nameExpr.splice), op.splice, c.Expr[Any](value).splice))
    }

    def makeComposite(f1: c.Expr[Filter], f2: c.Expr[Filter], operation: Name): c.Expr[CompositeFilter] = operation.decoded match {
      case "||" =>       reify(CompositeFilter(f1.splice, f2.splice, JoinOperation.OR))
      case "&&" =>       reify(CompositeFilter(f1.splice, f2.splice, JoinOperation.AND))
      case _ => sys.error("Failed to find operator")
    }

    def decompose(tree: Tree): c.Expr[Filter] = try { tree match {
      case Apply(Select(Apply(Select(Select(This(scalaTypeName), pName), augName), List(tree1)),operator),tree2) if (
        scalaTypeName == newTypeName("scala") &&
          pName == newTermName("Predef") &&
          augName == newTermName("augmentString")
        ) => decompose(Apply(Select(tree1, operator), tree2)) // Repackage

      case Apply(Select(t1 @ Apply(_ , _), operator), List(t2)) =>
        makeComposite(decompose(t1), decompose(t2), operator)

      case Apply(Select(firstTree, operation), List(secondTree)) =>
        try { makeFilter(operation, findPath(c)(firstTree, name), secondTree) } catch {
          // Try it backwards in case they did something like '4 == foo.bar'
          case e: MatchError => makeFilter(operation, findPath(c)(secondTree, name), firstTree)
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
