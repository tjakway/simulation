package com.jakway.term.test.framework.gen

import com.jakway.term.interpreter.{Eval, Interpreter}
import com.jakway.term.numeric.types.NumericType

trait HasInterpreter[N <: NumericType[M], M]
  extends HasNumericType[N, M] {

  lazy val interpreter: Interpreter = Eval[N, M](numericType).right.get
}
