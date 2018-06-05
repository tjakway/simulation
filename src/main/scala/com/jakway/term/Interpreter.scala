package com.jakway.term

import com.jakway.term.numeric.types.NumericType

/**
  *
  * @param n the interpreter takes an instance of the numeric type
  *          used to do actual calculations
  * @tparam N
  * @tparam M
  */
class Interpreter[N <: NumericType[M], M](n: NumericType[M]) {
  def eval(t: Term) = t match {
    case _ => ???
  }
}
