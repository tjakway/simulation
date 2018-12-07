package com.jakway.term.test.framework

import com.jakway.term.elements._
import com.jakway.term.interpreter.Raw
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

  def genRaw: Gen[Raw[N, M]] = genM.map(Raw(_))

  def genNumericTerm: Gen[NumericTerm[N, M]] = ???


  def genTerm: Gen[Term] = ???

  def genLiteral: Gen[Literal[N, M]] = genM.map(m => Literal(m.toString))

  //add in Negative somewhere
  def genLeaf: Gen[Term] = Gen.oneOf(genRaw, genLiteral, genVariable)

  def genBranch: Gen[Term] = {
    val binaryFunctions:
      Seq[(NumericTerm[N, M], NumericTerm[N, M]) => Term] =
      Seq(Add.apply, Subtract.apply, Multiply.apply, Divide.apply)

    def genBinaryTerm = for {
      left <- genNumericTerm
      right <- genNumericTerm
      f <- Gen.oneOf(binaryFunctions)
    } yield {
      f(left, right)
    }
  }

  def genNegative: Gen[Negative[N, M]] = genNumericTerm.map(Negative.apply)
}
