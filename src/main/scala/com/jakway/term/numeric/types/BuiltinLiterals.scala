package com.jakway.term.numeric.types

import com.jakway.term.numeric.errors.SimError
import com.jakway.term.numeric.types.SpecialLiterals.{HasSpecialLiterals, SpecialLiteral, SpecialLiteralReadErrors}

class BuiltinLiterals[M](
  val negativeOne: M,
  val zero: M,
  val specialLiterals: Map[SpecialLiteral, M])
  extends HasSpecialLiterals[M]

object BuiltinLiterals {
  def mkBuiltinLiterals[M](
      readLiteral: String => Either[SimError, M]):
  Either[SimError, BuiltinLiterals[M]] = {
    for {
      negativeOne <- readLiteral("-1")
      zero <- readLiteral("0")
      specialLiterals <- readSpecialLiterals[M](readLiteral)
    } yield {
      new BuiltinLiterals(negativeOne, zero, specialLiterals)
    }
  }

  private def readSpecialLiterals[M](
       readLiteral: String => Either[SimError, M]):
    Either[SimError, Map[SpecialLiteral, M]] = {

    val empty: Either[Seq[SimError], Seq[(SpecialLiteral, M)]] =
      Right(Seq())

    val res = SpecialLiterals.Values.specialLiterals.foldLeft(empty) {
      case (acc, thisValue) => {
        readLiteral(thisValue.canonicalName) match {
          case Right(r) => acc match {
              //ignore success if we've already encountered an error
            case Left(es) => Left(es)
              //otherwise accumulate successes
            case Right(rs) => Right(rs :+ (thisValue, r))
          }
          case Left(e) => acc match {
            case Left(es) => Left(es :+ e)
              //start accumulating errors the first time
              //we encounter one
            case Right(rs) => Left(Seq(e))
          }
        }
      }
    }

    res match {
      case Right(xs) => Right(xs.toMap)
      case Left(es) => Left(SpecialLiteralReadErrors(es))
    }
  }
}