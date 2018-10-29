package com.jakway.term.elements.util

import com.jakway.term.elements.HasSubterms.NewInstanceF
import com.jakway.term.elements.{HasSubterms, NumericTerm, Term}
import com.jakway.term.numeric.types.NumericType

object NewInstanceHelpers {
  def arity2MkNewInstance[N <: NumericType[M], M, X <: Term]
  (constructor: (NumericTerm[N, M], NumericTerm[N, M]) => X): NewInstanceF = {
    (withSubterms: Seq[Term]) => {
      val Seq (l, r) = HasSubterms.assertArity (2, withSubterms).take (2)
      constructor(HasSubterms.assertCast(l), HasSubterms.assertCast(r))
    }
  }

  def arity1MkNewInstance[N <: NumericType[M], M, X <: Term]
  (constructor: NumericTerm[N, M] => X): NewInstanceF = {

    (withSubterms: Seq[Term]) => {
      val Seq (a) = HasSubterms.assertArity(1, withSubterms).take (1)
      constructor(HasSubterms.assertCast(a))
    }
  }

  trait HasConstructorArgType[N <: NumericType[M], M] {
    type ConstructorArgType = NumericTerm[N, M]
  }

  trait Arity1MkNewInstance[N <: NumericType[M], M]
    extends HasConstructorArgType[N, M] {
    def mkNewInstance[X <: Term]: (ConstructorArgType => X) => NewInstanceF
    = NewInstanceHelpers.arity1MkNewInstance
  }

  trait Arity2MkNewInstance[N <: NumericType[M], M]
    extends HasConstructorArgType[N, M] {
    def mkNewInstance[X <: Term]: ((ConstructorArgType, ConstructorArgType) => X) => NewInstanceF
      = NewInstanceHelpers.arity2MkNewInstance
  }
}
