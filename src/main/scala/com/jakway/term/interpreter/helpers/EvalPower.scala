package com.jakway.term.interpreter.helpers

import com.jakway.term.elements.Power
import com.jakway.term.interpreter.Eval.{EvalHelper, EvalType}
import com.jakway.term.interpreter.{Interpreter, Raw}
import com.jakway.term.interpreter.Interpreter.SymbolTable
import com.jakway.term.numeric.types.{NumericType, SimError}

class EvalPower[N <: NumericType[M], M](
  val numericType: N)
  extends EvalHelper[Power[N, M]] {
  import Raw._

  override def apply(table: SymbolTable, recurse: Interpreter)
                    (power: Power[N, M]): EvalType =
    power match {
      case Power(Raw(base), Raw(exponent)) =>
        raw(numericType.pow(base)(exponent))
      case _ => Left(new SimError(
        s"Implementation error: match failed on power" +
          s" (should have been caught by the HasSubterms case of eval)" +
          s": $power"))
    }
}
