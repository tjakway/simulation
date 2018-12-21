package com.jakway.term.test.framework.gen

import com.jakway.term.numeric.types.NumericType
import org.scalacheck.Properties

abstract class BaseProperties[N <: NumericType[M], M](
  override val name: String, override val numericType: N)
  extends Properties(name)
    with HasNumericType[N, M]
    with HasInterpreter[N, M]
