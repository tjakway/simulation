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

  def genBranch: Gen[NumericTerm[N, M]] = {

    def genBinaryTerm: Gen[NumericTerm[N, M]] = {
      def genLogarithm: Gen[Logarithm[N, M]] = {
        def gtZero(r: Raw[N, M]): Boolean =
          numericType.comparator.compare(r.value,
            numericType.builtinLiterals.zero) == 1

        for {
          base <- genNumericTerm

          //TODO: need a better way to guarantee
          //that the generated term is >0 so we can test logarithms
          //more thoroughly... probably by restricting variables in a generated
          //tree then filtering for eval(_) > 0
          of <- genRaw.filter(gtZero)
        } yield Logarithm(base, of)
      }

      //can't include Logarithm in the list of binary functions
      //because it has domain restrictions
      val binaryFunctions:
        Seq[(NumericTerm[N, M], NumericTerm[N, M]) => NumericTerm[N, M]] =
        Seq(Add.apply, Subtract.apply, Multiply.apply, Divide.apply,
          Power.apply)
      val binaryTerm = for {
        left <- genNumericTerm
        right <- genNumericTerm
        f <- Gen.oneOf(binaryFunctions)
      } yield {
        f(left, right)
      }

      Gen.oneOf(binaryTerm, genLogarithm)
    }


    def genTrigFunction: Gen[NumericTerm[N, M]] = {

      val trigFunctions: Seq[NumericTerm[N, M] => NumericTerm[N, M]] =
        Seq(Sin.apply, Cos.apply, Tan.apply,
          Arcsin.apply, Arccos.apply, Arctan.apply)

      for {
        f <- Gen.oneOf(trigFunctions)
        n <- genNumericTerm
      } yield f(n)
    }

    def genNegative: Gen[Negative[N, M]] = genNumericTerm.map(Negative.apply)

    Gen.oneOf(genNegative, genBinaryTerm, genTrigFunction)
  }

}
