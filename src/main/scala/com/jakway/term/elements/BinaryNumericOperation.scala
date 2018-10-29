package com.jakway.term.elements

import com.jakway.term.elements.util.NewInstanceHelpers.Arity2MkNewInstance
import com.jakway.term.elements.util.InverseConstructorHelpers
import com.jakway.term.numeric.types.{NumericType, SimError}

trait BinaryNumericOperation[N <: NumericType[M], M]
  extends NumericOperation[N, M]
  with BinaryTerm[NumericTerm[N, M]]
  with Arity2MkNewInstance[N, M] {

  override val numArguments: Int = 2

  /**
    * mkInverseConstructorE for 2-arity types
    * @return
    */
  def mkInverseConstructorE:
    ((ConstructorArgType, ConstructorArgType) => Term) => Seq[Term] => Either[SimError, Term] =
    InverseConstructorHelpers.arity2MkInverseConstructorE
}
