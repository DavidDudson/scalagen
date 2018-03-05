package scala.meta.gen

import org.scalameta.scalagen.GeneratorContext

import scala.annotation.StaticAnnotation
import scala.meta._

abstract class Generator(val name: String, val ctx: Option[GeneratorContext] = None)
  extends StaticAnnotation
  with ExtensionGeneratorApi
  with CompanionGeneratorApi
  with ManipulationGeneratorApi
  with TransmutationGeneratorApi
  with ParameterGeneratorApi

/**
  * Use this trait for extending existing definitions.
  *
  * Example use case: Generating methods.
  *
  * Default: Add no new stats
  */
trait ExtensionGeneratorApi {
  self: Generator =>

  def extend(c: Defn.Class): List[Stat] = Nil
  def extend(t: Defn.Trait): List[Stat] = Nil
  def extend(o: Defn.Object): List[Stat] = Nil
}

object ExtensionGeneratorApi {
  def partialExtender(gen: ExtensionGeneratorApi): PartialFunction[Tree, List[Stat]] = {
    case c: Defn.Class => gen.extend(c)
    case t: Defn.Trait => gen.extend(t)
    case o: Defn.Object => gen.extend(o)
  }
}

object IdentityGenerator extends Generator("Identity")

/**
  * Use this trait for extending the case class of a Defn.
  *
  * Example use case: Deriving typeclass instances.
  *
  * Default: Add no new stats
  *
  * Note: These *will* generate a companion if one does not exist.
  */
trait CompanionGeneratorApi {
  self: Generator =>

  def extendCompanion(c: Defn.Class): List[Stat] = Nil
  def extendCompanion(c: Defn.Type): List[Stat] = Nil
  def extendCompanion(c: Defn.Trait): List[Stat] = Nil

  def extendCompanion(t: Decl.Type): List[Stat] = Nil
}

object CompanionGeneratorApi {
  def partialExtender(gen: CompanionGeneratorApi): PartialFunction[Tree, List[Stat]] = {
    case c: Defn.Class => gen.extendCompanion(c)
    case t: Defn.Trait => gen.extendCompanion(t)
    case t: Defn.Type => gen.extendCompanion(t)
    case t: Decl.Type => gen.extendCompanion(t)
  }
}

/**
  * Use this trait when you want full control
  * over the annotee, but still remain in a
  * blackbox style
  *
  * Example use case: Decorators
  *
  * Default: return the input
  */
trait ManipulationGeneratorApi {
  self: Generator =>

  def manipulate(c: Defn.Class): Defn.Class = c
  def manipulate(t: Defn.Trait): Defn.Trait = t
  def manipulate(t: Defn.Type): Defn.Type = t
  def manipulate(o: Defn.Object): Defn.Object = o
  def manipulate(d: Defn.Def): Defn.Def = d
  def manipulate(v: Defn.Val): Defn.Val = v
  def manipulate(v: Defn.Var): Defn.Var = v

  def manipulate(v: Decl.Var): Decl.Var = v
  def manipulate(v: Decl.Val): Decl.Val = v
  def manipulate(d: Decl.Def): Decl.Def = d
  def manipulate(t: Decl.Type): Decl.Type = t
}

object ManipulationGeneratorApi {
  def partialManipulator[A](gen: ManipulationGeneratorApi): PartialFunction[A, A] = {
    case c: Defn.Class => gen.manipulate(c).asInstanceOf[A]
    case c: Defn.Trait => gen.manipulate(c).asInstanceOf[A]
    case c: Defn.Type => gen.manipulate(c).asInstanceOf[A]
    case c: Defn.Object => gen.manipulate(c).asInstanceOf[A]
    case c: Defn.Def => gen.manipulate(c).asInstanceOf[A]
    case c: Defn.Val => gen.manipulate(c).asInstanceOf[A]
    case c: Defn.Var => gen.manipulate(c).asInstanceOf[A]
    case c: Decl.Var => gen.manipulate(c).asInstanceOf[A]
    case c: Decl.Val => gen.manipulate(c).asInstanceOf[A]
    case c: Decl.Def => gen.manipulate(c).asInstanceOf[A]
    case c: Decl.Type => gen.manipulate(c).asInstanceOf[A]
  }
}

/**
  * Black magic
  *
  * Use this when you want to generate a number of
  * Definitions inside your parent, or even remove the original
  *
  * This is not guaranteed to work for all cases.
  *
  * For example, generating a top level `Val` will be invalid
  *
  * Default: return the input
  */
trait TransmutationGeneratorApi {
  self: Generator =>

  def transmute(c: Defn.Class): List[Stat] = c :: Nil
  def transmute(t: Defn.Trait): List[Stat] = t :: Nil
  def transmute(t: Defn.Type): List[Stat] = t :: Nil
  def transmute(o: Defn.Object): List[Stat] = o :: Nil
  def transmute(d: Defn.Def): List[Stat] = d :: Nil
  def transmute(v: Defn.Val): List[Stat] = v :: Nil
  def transmute(v: Defn.Var): List[Stat] = v :: Nil

  def transmute(v: Decl.Var): List[Stat] = v :: Nil
  def transmute(v: Decl.Val): List[Stat] = v :: Nil
  def transmute(d: Decl.Def): List[Stat] = d :: Nil
  def transmute(t: Decl.Type): List[Stat] = t :: Nil
}


object TransmutationGeneratorApi {
  def partialTransmutator(gen: TransmutationGeneratorApi): PartialFunction[Tree, List[Stat]] = {
    case c: Defn.Class => gen.transmute(c)
    case c: Defn.Trait => gen.transmute(c)
    case c: Defn.Type => gen.transmute(c)
    case c: Defn.Object => gen.transmute(c)
    case c: Defn.Def => gen.transmute(c)
    case c: Defn.Val => gen.transmute(c)
    case c: Defn.Var => gen.transmute(c)
    case c: Decl.Var => gen.transmute(c)
    case c: Decl.Val => gen.transmute(c)
    case c: Decl.Def => gen.transmute(c)
    case c: Decl.Type => gen.transmute(c)
  }
}

/**
  * Used when you wish to generate stats based on a specific parameter.
  *
  * For example, adding assertion checks.
  *
  * Default: No stats added
  */
trait ParameterGeneratorApi {
  self: Generator =>
  def extend(p: Type.Param): List[Stat] = Nil
  def extend(p: Term.Param): List[Stat] = Nil
}


object ParameterGeneratorApi {
  def partialParameter(gen: ParameterGeneratorApi): PartialFunction[Tree, List[Stat]] = {
    case p: Type.Param => gen.extend(p)
    case p: Term.Param => gen.extend(p)
  }
}