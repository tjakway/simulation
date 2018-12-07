package com.jakway.term.test.framework

import com.jakway.term.elements.{NumericTerm, Term, Variable}
import com.jakway.term.numeric.types.NumericType
import org.scalacheck.{Arbitrary, Gen}

class ArbitraryTerm[N <: NumericType[M], M]
  (val numericType: N) {
  def genStr: Gen[String] = Gen.alphaNumStr

  def genVariable: Gen[Variable[N, M]] =
      for {
        name <- genStr
        desc <- genStr
        optDesc <- Gen.option(desc)
      } yield { Variable.apply(name, optDesc) }

  def genM: Gen[M] = Gen.numStr.map(
    numericType.readLiteral(_).right.get)

  def genNumericTerm: Gen[NumericTerm[N, M]] = ???


  def genTerm: Gen[Term] = ???

}
