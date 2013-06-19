package scalads.macroimpls

import scalads.core.{Transformer, EntityBacker}

import scalads._
import scalads.readers.ObjectReader
import scala.reflect.macros.Context
import scalads.writers.Writer


/**
 * @author Bryce Anderson
 *         Created on 6/9/13
 */
object EntityDeserializer {

  def extendWithEntityBacker[U: c.WeakTypeTag, E: c.WeakTypeTag]
  (c: Context)(ds: c.Expr[Datastore[_, E]], transExpr:c.Expr[Transformer[U, E]], reader: c.Expr[ObjectReader]): c.Expr[U with EntityBacker[U, E]] = {
    val helpers = new macrohelpers.MacroHelpers[c.type](c)
    import helpers.typeArgumentTree

    import c.universe._

    def constructorExtractor(tree: Tree): List[List[Tree]] = {
      def extract(tree: Tree, lst: List[List[Tree]]): List[List[Tree]] = tree match {
        case Apply(tree, args: List[_]) => extract(tree, args::lst)
        case _ => lst
      }
      extract(tree, Nil)
    }

    def buildObjParamExtract(tree: Tree) = {
      val Block(reader::newObjTree::Nil, _) = tree
      val ValDef(_, _, _, objConstructor) = newObjTree

      (constructorExtractor(objConstructor), reader)
    }

    val tpe = weakTypeOf[U]

    val readerName = newTermName(c.fresh("reader"))
    val readerExpr = c.Expr[ObjectReader](Ident(readerName))
    val newReaderTree = ValDef(Modifiers(), readerName, TypeTree(typeOf[ObjectReader]), reader.tree)

    val dsName = newTermName(c.fresh("datastore"))
    val dsExpr = c.Expr[ObjectReader](Ident(dsName))
    val newDatastoreTree = ValDef(Modifiers(), dsName, TypeTree(weakTypeOf[Datastore[_, E]]), ds.tree)

    val builderTree = macroimpls.Deserializer.deserializeImpl[U](c)(reader).tree

    val appliedTpeTree = typeArgumentTree(tpe)
    val TypeRef(_, backerSym, _) = weakTypeOf[EntityBacker[Any, E]]

    val (ctorTree: List[List[Tree]], readerTree) = buildObjParamExtract(builderTree)


    val updateTree = Serializer.serializeImpl[U](c)(
      c.Expr[U](Ident(newTermName("obj"))), c.Expr[Writer[E]](Ident(newTermName("writer")))
    ).tree

    val freshTpeName = newTypeName(c.fresh("$EntityBacker"))

    // Builds the augmentation methods
    val newTree = Block(List(
      readerTree: Tree,
      ClassDef(Modifiers(Flag.FINAL), freshTpeName, List(), Template(List(appliedTpeTree, AppliedTypeTree(Ident(backerSym), List(appliedTpeTree, TypeTree(weakTypeOf[E])))),
        ValDef(Modifiers(Flag.PRIVATE), newTermName("self"), TypeTree(), EmptyTree) , List(
          DefDef(Modifiers(), nme.CONSTRUCTOR, Nil, Nil::Nil, TypeTree(),
            Block(
              ctorTree.foldLeft[Tree](Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR))
              {(a,b: List[Tree]) => Apply(a,b)}::Nil,
              Literal(Constant(()))
            )
          ),
          // TODO: This will need to be changed as not all datastores will use Entities...
          ValDef(Modifiers(), newTermName("ds_entity"), TypeTree(weakTypeOf[E]), reify(readerExpr.splice.entity.asInstanceOf[E]).tree): Tree,
          ValDef(Modifiers(), newTermName("ds"), TypeTree(weakTypeOf[Datastore[_, E]]), dsExpr.tree),
          ValDef(Modifiers(), newTermName("transformer"), TypeTree(weakTypeOf[scalads.core.Transformer[U, E]]), transExpr.tree),
          DefDef(Modifiers(), newTermName("ds_serialize"), Nil, List(
            ValDef(Modifiers(Flag.PARAM), newTermName("obj"), typeArgumentTree(tpe), EmptyTree)::
              ValDef(Modifiers(Flag.PARAM), newTermName("writer"), TypeTree(typeOf[Writer[_]]), EmptyTree)::Nil
          ), TypeTree(typeOf[Writer[_]]), Block(updateTree::Nil, Ident(newTermName("writer"))))
        ))
      )),
      Apply(Select(New(Ident(freshTpeName)), nme.CONSTRUCTOR), List())
    )

    val result = c.Expr[U with EntityBacker[U, E]](  Block(newReaderTree::newDatastoreTree::Nil, newTree) )

    //println(result)
    result
  }
}
