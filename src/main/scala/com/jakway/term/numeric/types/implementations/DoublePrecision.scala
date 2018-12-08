package com.jakway.term.numeric.types.implementations


import java.util.Comparator

import com.jakway.term.numeric.errors.{CouldNotReadLiteralError, DivideByZeroError, LogarithmDomainError}
import com.jakway.term.numeric.types.NumericType.ReadLiteral
import com.jakway.term.numeric.types.SpecialLiterals.SpecialLiteralNotImplementedError
import com.jakway.term.numeric.types._

import scala.language.postfixOps
import scala.util.{Either, Left, Right}
import scala.{math => M}


object DoublePrecision {
  /**
    * ***Entry point***
    * returns an implementation of NumericType for Double
    * @return
    */
  def mkNumericType(): Either[SimError, NumericType[Double]] = {
    //parse builtins
    BuiltinLiterals.mkBuiltinLiterals[Double](readLiteralF)
      .map(new DoublePrecision(_))
  }

  private def readLiteralF: ReadLiteral[Double] = { x: String =>
    try {
      parseSpecialLiteral(x).getOrElse(Right(x.toDouble))
    } catch {
      case _: Throwable => Left(CouldNotReadLiteralError(x))
    }
  }

  /**
    * @param lit
    * @return Some if {@lit} was identified as a special literal:
    *             returns the result of parsing it as such
    *                (the result is an Either type)
    *         None if {@lit} was not identified as a special literal
    */
  private def parseSpecialLiteral(lit: String): Option[Either[SimError, Double]] = {
    if(SpecialLiterals.contains(lit)) {
      Some(if(SpecialLiterals.Values.e.isName(lit)) {
        Right(Math.E)
      } else if(SpecialLiterals.Values.pi.isName(lit)) {
        Right(Math.PI)
      } else {
        Left(SpecialLiteralNotImplementedError(lit))
      })
    } else {
      None
    }
  }
}


private class DoublePrecision(override val builtinLiterals: BuiltinLiterals[Double])
  extends NumericTypeImplementationHelper[Double] {
  override val sin: TrigFunction = total2(M.sin)
  override val cos: TrigFunction = total2(M.cos)
  override val tan: TrigFunction = total2(M.tan)
  override val arcsin: TrigFunction = total2(M.asin)
  override val arccos: TrigFunction = total2(M.acos)
  override val arctan: TrigFunction = total2(M.atan)

  override val pow: BinaryMathFunction = (x: Double) => (y: Double) => Right(M.pow(x, y))
  //override val root: BinaryMathFunction = total3(DoublePrecisionImplementation.root)

  override val log: BinaryMathFunction = (base: Double) => (of: Double) => {
    if(of <= 0) {
      Left(LogarithmDomainError(base.toString, of.toString))
    } else {
      Right(DoublePrecisionImplementation.log(base)(of))
    }
  }

  override val add: BinaryMathFunction = total3(DoublePrecisionImplementation.add)
  override val multiply: BinaryMathFunction = total3(DoublePrecisionImplementation.times)
  override val divide: BinaryMathFunction = DoublePrecisionImplementation.div

  override val readLiteral: ReadLiteral[Double] = DoublePrecision.readLiteralF

  override val comparator: Comparator[Double] = new Comparator[Double] {
    override def compare(x: Double, y: Double): Int = x.compareTo(y)
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

  def log(base: Double)(of: Double): Double = {
    if(base == 10) {
      Math.log10(of)
    } else if(base == Math.E) {
      Math.log(of)
    } else {
      //need to use change-of-base formula
      Math.log(of) / Math.log(base)
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

