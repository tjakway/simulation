package com.jakway.term.interpreter.helpers

import com.jakway.term.elements._
import com.jakway.term.interpreter.Eval.{EvalHelper, EvalType}
import com.jakway.term.interpreter.{Interpreter, Raw}
import com.jakway.term.interpreter.Interpreter.SymbolTable
import com.jakway.term.numeric.errors.DivideByZeroError
import com.jakway.term.numeric.types.NumericType

class EvalBinaryNumericOperation[N <: NumericType[M], M](
  val numericType: N)
  extends EvalHelper[BinaryNumericOperation[N, M]] {
  import Raw._

  val negativeOne: M = numericType.builtinLiterals.negativeOne
  val zero: M = numericType.builtinLiterals.zero

  override def apply(table: SymbolTable, recurse: Interpreter)
                    (z: BinaryNumericOperation[N, M]): EvalType =
  z match {
    case Add(Raw(l), Raw(r)) => raw(numericType.add(l)(r))
      //subtraction is a + (-1 * b)
    case Subtract(Raw(l), Raw(r)) =>
        raw(numericType.multiply(negativeOne)(r)
          .flatMap(numericType.add(l)))

    case Multiply(Raw(l), Raw(r)) =>
      raw(numericType.multiply(l)(r))

    case Divide(Raw(l), Raw(r)) => {
      if(r == zero) {
        Left(DivideByZeroError(l, r))
      } else {
        raw(numericType.divide(l)(r))
      }
    }
  }
}
