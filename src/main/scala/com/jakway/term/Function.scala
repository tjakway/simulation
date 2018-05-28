package com.jakway.term

import com.jakway.term.numeric.types.NumericType

trait Function[F] {
  def compute: F
}

/**
  * such that I is the inverse of F
  */
trait InvertibleFunction[F, I] extends Function[F] {
  def inverse: Function[I]
}
