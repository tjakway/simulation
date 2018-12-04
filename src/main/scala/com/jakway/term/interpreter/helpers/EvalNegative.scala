package com.jakway.term.interpreter.helpers

import com.jakway.term.elements.{Multiply, Negative, Term}
import com.jakway.term.interpreter.Eval.{EvalHelper, EvalType}
import com.jakway.term.interpreter.Interpreter.SymbolTable
import com.jakway.term.interpreter.{Interpreter, Raw}
import com.jakway.term.numeric.types.NumericType

/**
  * replace Negative(x) with (x * -1) and eval
 *
  * @param negativeOne
  * @tparam N
  * @tparam M
  */
class EvalNegative[N <: NumericType[M], M](val negativeOne: Raw[N, M])
  extends EvalHelper[Negative[N, M]] {

  def replaceWith(neg: Negative[N, M]): Term =
    Multiply(neg.arg, negativeOne)

  def apply(t: SymbolTable, recurse: Interpreter)
           (term: Negative[N, M]): EvalType =
    recurse.eval(t)(replaceWith(term))
}
