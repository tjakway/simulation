package com.jakway.term.numeric

import com.jakway.term.numeric.types.NumericType
import com.jakway.term.{DoubleArgFunctionApplication, InvertibleFunction, NumericFunctionApplication, NumericTerm}

trait NumericBinaryInvertibleFunction[N <: NumericType[M], M]
  extends InvertibleFunction[N, M, M => M => M, M => M => M] {

  def apply(first: NumericTerm[N, M], second: NumericTerm[N, M]):
    NumericFunctionApplication[N, M, M => M => M] =
      DoubleArgFunctionApplication(this, first, second)
}

trait NumericChiralBinaryInvertibleFunction[N <: NumericType[M], M]
  extends InvertibleFunction[N, M, M => M => M, M => M => M] {

  def apply(first: NumericTerm[N, M], second: NumericTerm[N, M]):
  NumericFunctionApplication[N, M, M => M => M] =
    DoubleArgFunctionApplication(this, first, second)
}
