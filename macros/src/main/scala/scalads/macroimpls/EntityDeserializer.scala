package scalads.macroimpls

import scalads.core.EntityBacker

import scalads._
import scalads.readers.{ObjectReader, GAEObjectReader}
import scala.reflect.macros.Context


/**
 * @author Bryce Anderson
 *         Created on 6/9/13
 */
object EntityDeserializer {

  def extendWithEntityBacker[U: c.WeakTypeTag](c: Context)(ds: c.Expr[Datastore], reader: c.Expr[ObjectReader]): c.Expr[U with EntityBacker[U]] = {
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
    val newDatastoreTree = ValDef(Modifiers(), dsName, TypeTree(typeOf[Datastore]), ds.tree)

    val builderTree = macroimpls.Deserializer.deserializeImpl[U](c)(reader).tree

    val appliedTpeTree = typeArgumentTree(tpe)
    val TypeRef(_, backerSym, _) = typeOf[EntityBacker[Any]]

    val (ctorTree: List[List[Tree]], readerTree) = buildObjParamExtract(builderTree)

    val updateTree = Serializer.serializeToEntityImpl[U](c)(
      c.Expr[U](Ident(newTermName("obj"))), c.Expr[Entity](Ident(newTermName("entity")))
    ).tree

    // Builds the augmentation methods
    val newTree = Block(List(
      readerTree: Tree,
      ClassDef(Modifiers(Flag.FINAL), newTypeName("$anon"), List(), Template(List(appliedTpeTree, AppliedTypeTree(Ident(backerSym), List(appliedTpeTree))),
        ValDef(Modifiers(Flag.PRIVATE), newTermName("self"), TypeTree(), EmptyTree) , List(
          DefDef(Modifiers(), nme.CONSTRUCTOR, Nil, Nil::Nil, TypeTree(),
            Block(
              ctorTree.foldLeft[Tree](Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR))
              {(a,b: List[Tree]) => Apply(a,b)}::Nil,
              Literal(Constant(()))
            )
          ),
          // TODO: This will need to be changed as not all datastores will use Entities...
          ValDef(Modifiers(), newTermName("ds_backingEntity"), TypeTree(typeOf[Entity]), reify(readerExpr.splice.asInstanceOf[GAEObjectReader].entity.asInstanceOf[Entity]).tree): Tree,
          ValDef(Modifiers(), newTermName("ds"), TypeTree(typeOf[Datastore]), dsExpr.tree),
          DefDef(Modifiers(), newTermName("ds_serialize"), Nil, List(
            ValDef(Modifiers(Flag.PARAM), newTermName("obj"), typeArgumentTree(tpe), EmptyTree)::
              ValDef(Modifiers(Flag.PARAM), newTermName("entity"), TypeTree(typeOf[Entity]), EmptyTree)::Nil
          ), TypeTree(typeOf[Unit]), updateTree)
        ))
      )),
      Apply(Select(New(Ident(newTypeName("$anon"))), nme.CONSTRUCTOR), List())
    )

    val result = c.Expr[U with EntityBacker[U]](  Block(newReaderTree::newDatastoreTree::Nil, newTree) )

    result

  }

}
