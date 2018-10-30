package com.jakway.term.interpreter.helpers

import com.jakway.term.elements.Literal
import com.jakway.term.interpreter.{Interpreter, Raw}
import com.jakway.term.interpreter.Interpreter.SymbolTable
import com.jakway.term.numeric.types.{NumericType, SimError}

class EvalHelpers[N <: NumericType[M], M](
     val negativeTerm: EvalNegative[N, M],
     val functionCall: EvalFunctionCall[N, M]
)

object EvalHelpers {
  def setup[N <: NumericType[M], M](
           readLiteral: Literal[N, M] => Either[SimError, Raw[N, M]],
           recurse: Interpreter,
           table: SymbolTable): Either[SimError, EvalHelpers[N, M]] = {
    for {
      negativeOne <- readLiteral(Literal[N, M]("-1"))
    } yield {
      new EvalHelpers[N, M](
        new EvalNegative[N, M](recurse, negativeOne),
        new EvalFunctionCall[N, M](recurse))
    }
  }
}