package org.scalameta.scalagen

import scala.meta.Tree

object LevelBasedTreeReducer {
  def reduce[A](tree: Tree, f: (Tree, Int) => A, reducer: (A, A) => A, level: Int = 0): A = {
    val all = f(level, tree) :: tree.children.map(reduce(_, f, reducer, level + 1))
    all.reduce(reducer)
  }
}
