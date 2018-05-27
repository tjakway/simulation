package com.jakway.term

import com.jakway.term.numeric.types.NumericType

trait Function[N <: NumericType[M], M, F] {
  def compute: F
}

/**
  * such that I is the inverse of F
  */
trait InvertibleFunction[N <: NumericType[M], M, F, I] extends Function[N, M, F] {
  def inverse: Function[N, M, I]
}
