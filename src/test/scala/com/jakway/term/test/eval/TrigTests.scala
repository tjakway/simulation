package com.jakway.term.test.eval

import com.jakway.term.elements.Sin
import com.jakway.term.interpreter.Raw
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.test.framework.InterpreterTest
import org.scalactic.Equality

class TrigTests[N <: NumericType[M], M]
  (override val numericType: N, implicit override val equality: Equality[M])
  extends InterpreterTest[N,M](numericType, equality) {

  "eval" should "give sin(0) = 0" in {
    eval(Sin(zero)) shouldEqual zero
  }
}
