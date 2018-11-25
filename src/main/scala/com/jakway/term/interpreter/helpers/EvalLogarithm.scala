package com.jakway.term.interpreter.helpers

import com.jakway.term.elements.Logarithm
import com.jakway.term.interpreter.Eval.{EvalHelper, EvalType}
import com.jakway.term.interpreter.{Eval, Interpreter, Raw}
import com.jakway.term.interpreter.Interpreter.SymbolTable
import com.jakway.term.numeric.types.NumericType

class EvalLogarithm[N <: NumericType[M], M](
  val recurse: Interpreter,
  val numericType: N)
  extends EvalHelper[Logarithm[N, M]](recurse) {

  override def apply(table: SymbolTable)
                    (z: Logarithm[N, M]): EvalType =

    z match {
      case Logarithm(l@Raw(base), r@Raw(of))
        if l.isInstanceOf[Raw[N, M]] && r.isInstanceOf[Raw[N, M]] =>
        (numericType.log(base.asInstanceOf[M])
        (of.asInstanceOf[M])).map(Raw.apply)
      case Logarithm(base, of) => {
        val errMsg = "Expected numeric term in evaluation of" +
          " Logarithm " + z.toString
        for {
          eBase <- recurse.eval(table)(base)
          nBase <- Eval.expectNumericTerm[N, M](errMsg, eBase)
          eOf <- recurse.eval(table)(of)
          nOf <- Eval.expectNumericTerm[N, M](errMsg, eOf)
          eLog <- recurse.eval(table)(Logarithm(nBase, nOf))
        } yield {
          eLog
        }
      }
    }

}
