package com.jakway.term.test

import com.jakway.term.numeric.types.NumericType

trait NumericTypeTest[N <: NumericType[M], M] {
  val numericType: N
}
