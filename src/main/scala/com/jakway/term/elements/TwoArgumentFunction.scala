package com.jakway.term.elements

import com.jakway.term.elements.util.NewInstanceHelpers.Arity2MkNewInstance
import com.jakway.term.elements.util.InverseConstructorHelpers
import com.jakway.term.numeric.types.{NumericType, SimError}

abstract class TwoArgumentFunction[N <: NumericType[M], M]
  (val arg1: Term, val arg2: Term)
  extends NumericFunction[N, M]
  with Arity2MkNewInstance[N, M] {

  override val arguments: Seq[Term] = Seq(arg1, arg2)
  override val subterms: Seq[Term] = arguments

  override val numArguments: Int = 2

  def mkInverseConstructorE: ((ConstructorArgType, ConstructorArgType) => Term) =>
    Seq[Term] => Either[SimError, Term] =
    InverseConstructorHelpers.arity2MkInverseConstructorE
}
