package org.scalameta.scalagen

import cats._
import cats.free._
import cats.implicits._
import monocle._

import scala.meta.{XtensionShow => _, _}
import scala.meta.gen._

sealed trait GeneratorContext {
  def input: Tree
  def generators: List[Generator]
}

case class GeneratorInputContext(input: Tree, generators: List[Generator]) extends GeneratorContext

case class GeneratorOutputContext(
                               previousStates: Tree,
                               appliedGenerator: Generator,
                               remainingGenerators: List[Generator],
                               out: Tree,
                               ) extends GeneratorContext {
}

object GeneratorTree {

  /**
    * Lazily build a Cofree from this tree.
    *
    * Cofree objects are only created as the tree is traversed.
    */
  def apply(t: Tree): GeneratorTreeF[Tree] =
    Cofree(t, Eval.later(t.children.map(apply)))

  /**
    * Produce a product prefix based tree representation.
    *
    * Includes all nodes of the tree.
    *
    * Defn.Class
    *  - Defn.Var
    *    - Defn.Def
    */
  implicit def treeShowInstance: Show[GeneratorTreeF[Tree]] =
    Show.show[GeneratorTreeF[Tree]](genTraversalString(regularTraversal[Tree], _, identity[Tree]))

  implicit def treeCtxShowInstance: Show[GeneratorTree] =
    _.map(_.input).show

  /**
    * Will print all nodes visited by the given traversal
    */
  def genTraversalString[A, B](t: Traversal[GeneratorTreeF[A], B], ot: GeneratorTreeF[A], f: B => Tree): String = {
    val childString =
      ot.tailForced
        .map(genTraversalString(t, _, f))
        .filterNot(_.isEmpty)
        .flatMap(_.lines.toList)
        .mkString("\n  ")

    val res = t.headOption(ot) match {
      case None if childString.isEmpty =>
        ""
      case None =>
        error(childString)
        childString
      case Some(a) if childString.isEmpty =>
        s" - ${treePrefixAndName(f(a))}"
      case Some(a) =>
        s""" - ${treePrefixAndName(f(a))}
           |  $childString""".stripMargin
    }

    res
  }

  /**
    * Primarily used for debug
    *
    * For example
    * "Defn.Class: Foo"
    *
    * TODO: Make an extract/replace instance for names
    */
  private def treePrefixAndName(tree: Tree) = {
    val nameStr =
      tree match {
        case o: Pkg => o.ref.syntax + "." + o.name.syntax
        case o: Defn.Object => o.name.syntax
        case c: Defn.Class => c.name.syntax
        case t: Defn.Trait => t.name.syntax
        case t: Defn.Type => t.name.syntax
        case t: Decl.Type => t.name.syntax
        case d: Defn.Def => d.name.syntax
        case d: Decl.Def => d.name.syntax
        case v: Defn.Val => genNameSyntax(v.pats)
        case v: Decl.Val => genNameSyntax(v.pats)
        case v: Defn.Var => genNameSyntax(v.pats)
        case v: Decl.Var => genNameSyntax(v.pats)
        case s: Term.Select => s.syntax
        case s: Type.Select => s.syntax
        case n: Term.Name => n.syntax
        case n: Type.Name => n.syntax
        case _ => ""
      }
    if (nameStr.isEmpty) {
      tree.productPrefix
    } else {
      tree.productPrefix + s": $nameStr"
    }
  }

  private def genNameSyntax(pats: List[Pat]): String =
    pats
      .collect({ case Pat.Var(name) => name })
      .mkString(", ")

  /**
    * OwnerTree is just simple tree with an arbritraty ,
    * The issue is we cannot use the name Tree as we do not want
    * conflicts with scalameta.tree
    *
    * TODO: Consider moving this out of scalagen
    */
  type GeneratorTree = Cofree[List, GeneratorContext]

  /**
    * Partially applied alias for OwnerTree. Allows use as a Functor/Monad etc.
    */
  type GeneratorTreeF[A] = Cofree[List, A]

  def regularTraversal[A]: Traversal[GeneratorTreeF[A], A] =
    Traversal.fromTraverse[GeneratorTreeF, A](Traverse[GeneratorTreeF])

  def generatorInputPrism(gs: Set[Generator]): Prism[GeneratorTreeF[Tree], GeneratorTree] =
    Prism[GeneratorTreeF[Tree], GeneratorTree](t => {
      Option[GeneratorTree](t.map(GeneratorInputContext(_, gatherGenerators(t.head, gs))))
        .filter(_.head.generators.nonEmpty)
    })(_.map(_.input))

}
