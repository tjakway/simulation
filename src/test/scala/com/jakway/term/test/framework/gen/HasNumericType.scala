package com.jakway.term.test.framework.gen

import com.jakway.term.numeric.types.NumericType

trait HasNumericType[N <: NumericType[M], M] {
  val numericType: N
}
