package com.jakway.term.test.framework

import com.jakway.term.numeric.types.NumericType
import org.scalactic.Equality

class InterpreterTest[N <: NumericType[M], M]
  (implicit val equality: Equality[M]) {

}
