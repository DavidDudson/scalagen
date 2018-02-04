package org.scalameta.scalagen

import cats.implicits._
import org.scalameta.scalagen.GeneratorTree._
import org.scalatest.{FunSuite, Matchers}

import scala.meta.Source
import scala.meta.quasiquotes._

class GeneratorTreeTests extends FunSuite with Matchers {

  test("Proof that generator tree tailForced is identical to calling children") {
    val obj = q"object Foo"
    val gTreeChildren = GeneratorTree(obj).tailForced.map(_.head)
    assert(obj.children === gTreeChildren)
  }

  test("Show on very small tree") {
    val expected =
      """ - Defn.Object: Foo
        |   - Term.Name: Foo
        |   - Template
        |     - Self
        |       - Name.Anonymous""".stripMargin

    val shown = GeneratorTree(q"object Foo").show

    withClue("\nShown:\n" + shown + "\n\n") {
      assert(expected == shown)
    }
  }

  val src: Source =
    source"""package org.scala.meta

             class Baz(a: Int, b: Foo) {
               def blargh = {
                 val foo = {
                   case object Bar {
                     val foo = new Bar {
                        val baz = () => {
                          val bar = 2
                        }
                     }
                   }
                 }
               }
             }

             @DeleteMe
             object Foo {
               def bar = 2
             }"""

  val raw: String =
    """ - Source
      |   - Pkg: org.scala.meta.meta
      |     - Term.Select: org.scala.meta
      |       - Term.Select: org.scala
      |         - Term.Name: org
      |         - Term.Name: scala
      |       - Term.Name: meta
      |     - Defn.Class: Baz
      |       - Type.Name: Baz
      |       - Ctor.Primary
      |         - Name.Anonymous
      |         - Term.Param
      |           - Term.Name: a
      |           - Type.Name: Int
      |         - Term.Param
      |           - Term.Name: b
      |           - Type.Name: Foo
      |       - Template
      |         - Self
      |           - Name.Anonymous
      |         - Defn.Def: blargh
      |           - Term.Name: blargh
      |           - Term.Block
      |             - Defn.Val: foo
      |               - Pat.Var
      |                 - Term.Name: foo
      |               - Term.Block
      |                 - Defn.Object: Bar
      |                   - Mod.Case
      |                   - Term.Name: Bar
      |                   - Template
      |                     - Self
      |                       - Name.Anonymous
      |                     - Defn.Val: foo
      |                       - Pat.Var
      |                         - Term.Name: foo
      |                       - Term.NewAnonymous
      |                         - Template
      |                           - Init
      |                             - Type.Name: Bar
      |                             - Name.Anonymous
      |                           - Self
      |                             - Name.Anonymous
      |                           - Defn.Val: baz
      |                             - Pat.Var
      |                               - Term.Name: baz
      |                             - Term.Function
      |                               - Term.Block
      |                                 - Defn.Val: bar
      |                                   - Pat.Var
      |                                     - Term.Name: bar
      |                                   - Lit.Int
      |     - Defn.Object: Foo
      |       - Mod.Annot
      |         - Init
      |           - Type.Name: DeleteMe
      |           - Name.Anonymous
      |       - Term.Name: Foo
      |       - Template
      |         - Self
      |           - Name.Anonymous
      |         - Defn.Def: bar
      |           - Term.Name: bar
      |           - Lit.Int""".stripMargin

  val generators: String =
    """ - Defn.Object: Foo""".stripMargin

  val ot = GeneratorTree(src)

  test("Raw") {
    withClue(s"""
                |=====================
                |      Expected
                |=====================
                |$raw
                |=====================
                |      Actual
                |=====================
                |${ot.show}
                |=====================
                |""".stripMargin) {
      assert(raw == ot.show)
    }
  }

  test("Generators") {

    val str = GeneratorTree.genTraversalString(generatorTraversal(Set(DeleteMe)), ot)

    withClue(s"""
                |=====================
                |      Expected
                |=====================
                |$generators
                |=====================
                |      Actual
                |=====================
                |$str
                |=====================
        """.stripMargin) {

      assert(generators == str)
    }
  }

  test("Gather Generators given single generator") {
    val clazz = q"@DeleteMe class Foo"

    val generators = GeneratorTree.gatherGenerators(clazz, Set(DeleteMe))

    generators should contain only DeleteMe
  }

  test("Gather Generators given duplicate generator") {
    val clazz = q"@DeleteMe @DeleteMe class Foo"

    val generators = GeneratorTree.gatherGenerators(clazz, Set(DeleteMe))

    generators.size should equal(2)
  }

  test("Gather Generators given multiple generator") {
    val clazz = q"@PrintHi @DeleteMe class Foo"

    val generators = GeneratorTree.gatherGenerators(clazz, Set(PrintHi, DeleteMe))

    generators should contain inOrderOnly (PrintHi, DeleteMe)
  }

  test("Gather Generators given multiple annotation but single generator") {
    val clazz = q"@PrintHi @DeleteMe class Foo"

    val generators = GeneratorTree.gatherGenerators(clazz, Set(DeleteMe))

    generators should contain only DeleteMe
  }
}
