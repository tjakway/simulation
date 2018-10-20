package com.jakway.term.test

import com.jakway.term.numeric.types.NumericType
import com.jakway.term.numeric.types.implementations.DoublePrecision

package double {
  object Double {
    type M = Double
    type N = NumericType[Double]
    val inst = DoublePrecision
  }
  import Double._

  class DoubleTestHasSubterms extends TestHasSubterms[N, M](inst)
  class DoubleTestFindVariables extends TestFindVariables[N, M](inst)
}
