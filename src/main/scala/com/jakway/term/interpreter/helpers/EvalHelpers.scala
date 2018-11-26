package com.jakway.term.interpreter.helpers

import com.jakway.term.elements.Literal
import com.jakway.term.interpreter.{Interpreter, Raw}
import com.jakway.term.interpreter.Interpreter.SymbolTable
import com.jakway.term.numeric.types.{NumericType, SimError}

/**
  * supertype for term-specific eval implementations
  * @param negativeTerm
  * @param functionCall
  * @tparam N
  * @tparam M
  */
class EvalHelpers[N <: NumericType[M], M](
     val negativeTerm: EvalNegative[N, M],
     val functionCall: EvalFunctionCall[N, M],
     val binaryNumericOperation: EvalBinaryNumericOperation[N, M],
     val trigFunctions: EvalTrigFunctions[N, M],
     val logarithm: EvalLogarithm[N, M],
     val power: EvalPower[N, M]
)

object EvalHelpers {
  /**
    * warning: if you define an Interpreter lazily and pass itself
    * for recurse you will get a stack overflow as it keeps trying to recompute
    * itself
    * @param readLiteral
    * @param recurse
    * @param numericType
    * @tparam N
    * @tparam M
    * @return
    */
  def setup[N <: NumericType[M], M](
           readLiteral: Literal[N, M] => Either[SimError, Raw[N, M]],
           recurse: Interpreter,
           numericType: N): Either[SimError, EvalHelpers[N, M]] = {
    for {
      negativeOne <- readLiteral(Literal[N, M]("-1"))
    } yield {
      new EvalHelpers[N, M](
        new EvalNegative[N, M](recurse, negativeOne),
        new EvalFunctionCall[N, M](recurse),
        new EvalBinaryNumericOperation[N, M](recurse, numericType),
        new EvalTrigFunctions[N, M](recurse, numericType),
        new EvalLogarithm[N, M](recurse, numericType),
        new EvalPower[N, M](recurse, numericType))
    }
  }

  /**
    * overloaded on readLiteral (different name to prevent double definition)
    * @param readLiteralStr
    * @param recurse
    * @param numericType
    * @tparam N
    * @tparam M
    * @return
    */
  def setupWithReadLiteralStr[N <: NumericType[M], M](
           readLiteralStr: String => Either[SimError, M],
           recurse: Interpreter,
           numericType: N): Either[SimError, EvalHelpers[N, M]] = {

    def readLiteral(l: Literal[N, M]): Either[SimError, Raw[N, M]] =
      readLiteralStr(l.value).map(Raw.apply(_))

    setup(readLiteral _, recurse, numericType)
  }
}