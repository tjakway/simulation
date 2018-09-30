package com.jakway.term.numeric.types.implementations


import com.jakway.term
import com.jakway.term.numeric.errors.{CouldNotReadLiteralError, DivideByZeroError}
import com.jakway.term.numeric.types
import com.jakway.term.numeric.types.{NumericType, NumericTypeImplementation, SimError}

import scala.util.{Either, Left, Right}
import scala.{math => M}
import scala.language.postfixOps


object DoublePrecision extends NumericTypeImplementation[Double] {
  override val sin: TrigFunction = total2(M.sin)
  override val cos: TrigFunction = total2(M.cos)
  override val tan: TrigFunction = total2(M.tan)
  override val arcsin: TrigFunction = total2(M.asin)
  override val arccos: TrigFunction = total2(M.acos)
  override val arctan: TrigFunction = total2(M.atan)

  override val pow: BinaryMathFunction = (x: Double) => (y: Double) => Right(M.pow(x, y))
  override val root: BinaryMathFunction = total3(DoublePrecisionImplementation.root)
  override val add: BinaryMathFunction = total3(DoublePrecisionImplementation.add)
  override val times: BinaryMathFunction = total3(DoublePrecisionImplementation.times)
  override val div: BinaryMathFunction = DoublePrecisionImplementation.div

  override val readLiteral: String => Either[SimError, Double] = { x: String =>
    try {
      Right(x.toDouble)
    } catch {
      case _: Throwable => Left(CouldNotReadLiteralError(x))
    }
  }
}

private object DoublePrecisionImplementation {
  /**
    * takes the nth root of under
    * @param under would be passed to e.g. sqrt ("under the radical")
    * @param n the root to take (e.g. 2 for square root, 3 for cube root)
    * @return
    */
  def root(under: Double)(n: Double) = {
    //convenient built-ins for 2 and 3
    if(n == 2) {
      Math.sqrt(under)
    } else if(n == 3) {
      Math.cbrt(under)
    } else {
      //from https://stackoverflow.com/questions/6325793/nth-root-implementation
      Math.pow(Math.E, Math.log(under) / n)
    }
  }

  def add(a: Double)(b: Double) = a + b
  def times(a: Double)(b: Double) = a * b

  def div(a: Double)(b: Double): Either[SimError, Double] = if(b == 0) {
    Left(DivideByZeroError(a, b))
  } else {
    Right(a / b)
  }
}
