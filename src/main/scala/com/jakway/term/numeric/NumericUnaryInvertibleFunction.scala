package com.jakway.term.numeric

import com.jakway.term.numeric.types.NumericType
import com.jakway.term.{InvertibleFunction, NumericFunctionApplication, NumericTerm, SingleArgFunctionApplication}

trait NumericUnaryInvertibleFunction[N <: NumericType[M], M]
  extends InvertibleFunction[N, M, M => M, M => M] {

  def apply(in: NumericTerm[N, M]):
    NumericFunctionApplication[N, M, M => M] =
      SingleArgFunctionApplication(this, in)
}
