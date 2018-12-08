package com.jakway.term.test.framework.gen

import com.jakway.term.interpreter.{Eval, Interpreter}
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.test.framework.gen.HasInterpreter.{EvalInitializationError, NullNumericTypeError}

trait HasInterpreter[N <: NumericType[M], M]
  extends HasNumericType[N, M] {

  lazy val interpreter: Interpreter = {
    if(numericType == null) {
      throw NullNumericTypeError
    } else {
      Eval[N, M](numericType) match {
        case Right(x) => x
        case Left(e) => throw EvalInitializationError(e.toString)
      }
    }
  }
}

object HasInterpreter {
  case object NullNumericTypeError
    extends GenError("numericType from HasNumericType was null")

  case class EvalInitializationError(val errMsg: String)
    extends GenError("Eval.apply returned " + errMsg)
}
