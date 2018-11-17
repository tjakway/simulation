package com.jakway.term.elements

import com.jakway.term.elements.util.NewInstanceHelpers.Arity1MkNewInstance
import com.jakway.term.elements.util.InverseConstructorHelpers
import com.jakway.term.numeric.types.{NumericType, SimError}

trait OneArgumentFunction[N <: NumericType[M], M]
  extends NumericFunction[N, M]
  with Arity1MkNewInstance[N, M] {
  //trig functions only take 1 argument
  val argument: NumericTerm[N, M]
  override val arguments: Seq[Term] = Seq(argument)

  override val subterms: Seq[Term] = Seq(argument)

  override val numArguments: Int = 1

  def mkInverseConstructorE:
  (ConstructorArgType => Term) => Seq[Term] => Either[SimError, Term] =
    InverseConstructorHelpers.arity1MkInverseConstructorE
}
