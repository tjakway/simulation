package com.jakway.term

import java.lang.{Math => M}
import com.jakway.term.NumericTypeFactory.TrigFunctions
import scala.language.postfixOps

trait Function[N <: NumericType[M], M, F] {
  def compute: F
}

/**
  * such that I is the inverse of F
  */
trait InvertibleFunction[N <: NumericType[M], M, F, I] extends Function[N, M, F] {
  def inverse: Function[N, M, I]
}

trait NumericUnaryInvertibleFunction[N <: NumericType[M], M]
  extends InvertibleFunction[N, M, M => M, M => M] {

  def apply(in: NumericTerm[N, M]):
    NumericFunctionApplication[N, M, M => M] =
      SingleArgFunctionApplication(this, in)
}

trait NumericBinaryInvertibleFunction[N <: NumericType[M], M]
  extends InvertibleFunction[N, M, M => M => M, M => M => M] {

  def apply(first: NumericTerm[N, M], second: NumericTerm[N, M]):
    NumericFunctionApplication[N, M, M => M => M] =
      DoubleArgFunctionApplication(this, first, second)
}

trait NumericType[A] {
  type TrigFunction = NumericUnaryInvertibleFunction[NumericType[A], A]
  val sin: TrigFunction
  val cos: TrigFunction
  val tan: TrigFunction

  val arcsin: TrigFunction
  val arccos: TrigFunction
  val arctan: TrigFunction

  type BinaryMathFunction = NumericBinaryInvertibleFunction[NumericType[A], A]
  val pow: BinaryMathFunction
  val root: BinaryMathFunction
}

object NumericTypeFactory {
  //TODO: think about how to handle inverses
  case class ArithmeticOperations[A](
                                   // plus: A => A => A,
                                   // times: A => A => A
                                    )

  case class TrigFunctions[A](
      sin: A => A,
      cos: A => A,
      tan: A => A,

      arcsin: A => A,
      arccos: A => A,
      arctan: A => A)
}

/**
  * take functions, produce term objects
  * @param t
  * @tparam A
  */
class NumericTypeFactory[A](t: NumericTypeFactory.TrigFunctions[A],
                            pPow: A => A => A,
                            pRoot: A => A => A) extends NumericType[A] {
  //trig functions

  override val sin: TrigFunction =
    new TrigFunction {
      override def inverse = arcsin

      override def compute = t.sin
    }

  override val cos: TrigFunction =
    new TrigFunction {
      override def inverse = arccos

      override def compute = t.cos
    }

  override val tan: TrigFunction =
    new TrigFunction {
      override def inverse = arctan

      override def compute = t.tan
    }

  override val arcsin: TrigFunction =
    new TrigFunction {
      override def inverse = sin

      override def compute = t.arcsin
    }


  override val arccos: TrigFunction =
    new TrigFunction {
      override def inverse = cos

      override def compute = t.arccos
    }

  override val arctan: TrigFunction =
    new TrigFunction {
      override def inverse = tan

      override def compute = t.arctan
    }

  //other misc. things
  override val pow: BinaryMathFunction =
    new BinaryMathFunction {
      override def compute = pPow

      override def inverse = root
    }


  override val root: BinaryMathFunction =
    new BinaryMathFunction {
      override def compute: A => A => A = pRoot

      override def inverse = pow
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

object DoublePrecision extends NumericTypeFactory[Double](
  TrigFunctions(
    M.sin, M.cos, M.tan,
    M.asin, M.acos, M.atan
  ),
  DoublePrecisionImplementation.power _ curried,
  DoublePrecisionImplementation.root _ curried) {

}

class EnvParameters {
}
