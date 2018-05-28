package com.jakway.term.numeric.types.implementations


import com.jakway.term
import com.jakway.term.{ChiralInvertible, InvertibleFunction, Term}
import com.jakway.term.numeric.types.{NumericType, NumericTypeFactory}
import com.jakway.term.numeric.types.NumericTypeFactory.TrigFunctions

import scala.{math => M}
import scala.language.postfixOps

abstract class PlusTerm[N <: NumericType[M], M] extends ChiralInvertible[N => N] {

  type SingleArgFunction = M => M

  class Minus(right: Term, inverseF: term.Function[SingleArgFunction])
    extends InvertibleFunction[SingleArgFunction, SingleArgFunction] {

    override def compute: SingleArgFunction = (a: M) => a - right

    override def inverse: term.Function[SingleArgFunction] = inverseF
  }




}

object DoublePrecision extends NumericTypeFactory[Double](
  TrigFunctions(
    M.sin, M.cos, M.tan,
    M.asin, M.acos, M.atan
  ),
  DoublePrecisionImplementation.power _ curried,
  DoublePrecisionImplementation.root _ curried) {


  override val plus: ChiralBinaryMathFunction = new ChiralBinaryMathFunction {
    override def inverse: term.Function[NumericType[Double], Double, Double => Double => Double] = ???

    override def compute: Double => Double => Double = ???
  }
}

private object DoublePrecisionImplementation {
  /**
    * takes the nth root of under
    * @param under would be passed to e.g. sqrt ("under the radical")
    * @param n the root to take (e.g. 2 for square root, 3 for cube root)
    * @return
    */
  def root(under: Double, n: Double) = {
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

  def power(base: Double, exp: Double) = Math.pow(base, exp)
}
