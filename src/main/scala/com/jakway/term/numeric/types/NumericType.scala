package com.jakway.term.numeric.types

import java.math.MathContext
import java.util.Comparator

import com.jakway.term.elements.{Literal, NumericTerm}
import com.jakway.term.interpreter.Raw
import com.jakway.term.numeric.errors.{CouldNotReadLiteralError, DomainError, SimError, TrigDomainError}
import com.jakway.term.numeric.types.SpecialLiterals.{HasSpecialLiterals, SpecialLiteral, SpecialLiteralNotImplementedError, SpecialLiteralReadErrors}
import com.jakway.term.numeric.types.implementations.{BigDecimalPrecision, DoublePrecision}

trait NumericType[M] {
  import NumericType._
  type UnaryFunction = M => Either[SimError, M]
  type TrigFunction = UnaryFunction

  val sin: TrigFunction
  val cos: TrigFunction
  val tan: TrigFunction

  val arcsin: TrigFunction
  val arccos: TrigFunction
  val arctan: TrigFunction

  type BinaryMathFunction = M => M => Either[SimError, M]

  val pow: BinaryMathFunction
  val log: BinaryMathFunction

  val add: BinaryMathFunction
  val multiply: BinaryMathFunction
  val divide: BinaryMathFunction

  val readLiteral: ReadLiteral[M]
  val builtinLiterals: BuiltinLiterals[M]

  val comparator: Comparator[M]
}

object NumericType {
  type ReadLiteral[M] = String => Either[SimError, M]

  object Implementations {
    def getDoublePrecisionNumericTypeImplementation():
      Either[SimError, NumericType[Double]] =
      DoublePrecision.mkNumericType

    def getBigDecimalNumericTypeImplementation(
          mc: MathContext = BigDecimalPrecision.defaultMathContext):
      Either[SimError, NumericType[java.math.BigDecimal]] =
      BigDecimalPrecision.mkNumericType(mc)
  }

  object AllTypes {
    //smallest value guaranteed to be representable in all types
    lazy val smallestMinOfAllTypes: String =
      Double.MinValue.toString

    //largest value guaranteed to be representable in all types
    lazy val smallestMaxOfAllTypes: String =
      Double.MaxValue.toString
  }
}





trait NumericTypeImplementationHelper[M] extends NumericType[M] {
  /**
    * helper method for functions that can't fail
    * @param f
    * @return
    */
  def total2(f: M => M): M => Either[SimError, M] = (x: M) => Right(f(x))
  def total3(f: M => M => M): M => M => Either[SimError, M] =
    (x: M) => (y: M) => Right(f(x)(y))

  private def checkTrigDomain(trigFunctionName: String,
                              lower: M,
                              upper: M)(arg: M): Either[SimError, M] = {

    val lowerError: Option[String] = {
      //must be >= lower
      if(comparator.compare(arg, lower) >= 0) {
        Some(s"$arg < $lower")
      } else {
        None
      }
    }

    val upperError: Option[String] = {
      //must be <= upper
      if(comparator.compare(arg, upper) <= 0) {
        Some(s"$arg < $lower")
      } else {
        None
      }
    }

    val errors = Seq(lowerError, upperError)

    //prepend the prefix to all error messages
    val prefix = s"Domain error in $trigFunctionName: "
    errors.map(e => e.map(prefix + _))

    val empty: Either[SimError, M] = Right(arg)
    errors.foldLeft(empty) {
      case (Left(e), _) => Left(e)
      case (_, Some(e)) => Left(new TrigDomainError(e))
      case (Right(x), None) => Right(x)
    }
  }

  def checkAsinDomain: M => Either[SimError, M] =
    checkTrigDomain("Asin", builtinLiterals.negativeOne, builtinLiterals.one)

  def checkAcosDomain: M => Either[SimError, M] =
    checkTrigDomain("Acos", builtinLiterals.negativeOne, builtinLiterals.one)


}

