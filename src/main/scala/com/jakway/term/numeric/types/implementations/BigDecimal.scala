package com.jakway.term.numeric.types.implementations


import java.math.{MathContext, RoundingMode}

import ch.obermuhlner.math.big.BigDecimalMath
import com.jakway.term.numeric.types.NumericType.ReadLiteral
import com.jakway.term.numeric.types._
import java.math.BigDecimal
import java.util.Comparator

import com.jakway.term.numeric.errors.{CouldNotReadLiteralError, LogarithmDomainError, SimError}
import com.jakway.term.numeric.types.SpecialLiterals.SpecialLiteralNotImplementedError

/**
  * TODO: DRY re: DoublePrecision
  */
object BigDecimalPrecision {
  lazy val defaultMathContext: MathContext = scala.math.BigDecimal.defaultMathContext

  def mkNumericType(
       mc: MathContext = defaultMathContext):
  Either[SimError, NumericType[BigDecimal]] = {
    //parse builtins
    BuiltinLiterals.mkBuiltinLiterals[BigDecimal](readLiteralF(mc))
      .map(new BigDecimalPrecision(_, mc))
  }

  private def readLiteralF: MathContext => ReadLiteral[BigDecimal] = {
    mc: MathContext => x: String =>
    try {
      parseSpecialLiteral(mc, x).getOrElse(Right(new BigDecimal(x, mc)))
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
  private def parseSpecialLiteral(mc: MathContext, lit: String):
    Option[Either[SimError, BigDecimal]] = {
    if(SpecialLiterals.contains(lit)) {
      Some(if(SpecialLiterals.Values.e.isName(lit)) {
        Right(BigDecimalMath.e(mc))
      } else if(SpecialLiterals.Values.pi.isName(lit)) {
        Right(BigDecimalMath.pi(mc))
      } else {
        Left(SpecialLiteralNotImplementedError(lit))
      })
    } else {
      None
    }
  }
}

private class BigDecimalPrecision(
               override val builtinLiterals: BuiltinLiterals[BigDecimal],
               val mc: MathContext)
  extends NumericTypeImplementationHelper[BigDecimal] {

  def this(builtinLiterals: BuiltinLiterals[BigDecimal],
           precision: Int) {
    this(builtinLiterals, new MathContext(precision))
  }

  def this(builtinLiterals: BuiltinLiterals[BigDecimal],
           precision: Int,
           roundingMode: RoundingMode) {
    this(builtinLiterals, new MathContext(precision, roundingMode))
  }

  override val sin: TrigFunction = total2(BigDecimalMath.sin(_, mc))
  override val cos: TrigFunction = total2(BigDecimalMath.cos(_, mc))
  override val tan: TrigFunction = total2(BigDecimalMath.tan(_, mc))
  override val arcsin: TrigFunction = a => checkAsinDomain(a).map(BigDecimalMath.asin(_, mc))
  override val arccos: TrigFunction = a => checkAcosDomain(a).map(BigDecimalMath.acos(_, mc))
  override val arctan: TrigFunction = total2(BigDecimalMath.atan(_, mc))
  override val pow: BinaryMathFunction =
    total3(x => y => BigDecimalMath.pow(x, y, mc))

  override val log: BinaryMathFunction = {
    (base: BigDecimal) =>
    (of: BigDecimal) =>
      val cmp: Int = of.compareTo(builtinLiterals.zero)
      val ltOrEqToZero: Boolean = cmp == -1 || cmp == 0

      if(ltOrEqToZero) {
        Left(LogarithmDomainError(base.toString, of.toString))
      } else {
        Right(BigDecimalPrecisionImplementation.log(base, of, mc))
      }
  }

  override val add: BinaryMathFunction =
    total3((x: BigDecimal) => (y: BigDecimal) => x.add(y, mc))

  override val multiply: BinaryMathFunction =
    total3((x: BigDecimal) => (y: BigDecimal) => x.multiply(y, mc))

  override val divide: BinaryMathFunction =
    total3((x: BigDecimal) => (y: BigDecimal) => x.divide(y, mc))

  override val readLiteral: ReadLiteral[BigDecimal] =
    BigDecimalPrecision.readLiteralF(mc)

  override val comparator: Comparator[BigDecimal] = new Comparator[BigDecimal] {
    override def compare(x: BigDecimal, y: BigDecimal): Int =
      x.compareTo(y)
  }
}

object BigDecimalPrecisionImplementation {
  def log(base: BigDecimal, of: BigDecimal, mc: MathContext): BigDecimal = {
    if(base == 10) {
      BigDecimalMath.log10(base, mc)
    } else if(base == 2) {
      BigDecimalMath.log2(of, mc)
    } else if(base == Math.E) {
      BigDecimalMath.log(of, mc)
    } else {
      //need to use change-of-base formula
        BigDecimalMath.log(of, mc)
            .divide(BigDecimalMath.log(base, mc), mc)
    }
  }
}
