package com.jakway.term.test.gen

import com.jakway.term.elements.{IdentityFunction, NumericTerm, Term}
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.test.framework.gen.GenTermTrait
import org.scalacheck.Gen
import org.scalatest.{FlatSpec, Matchers}

/**
  * Sanity checks for generators
  * @param numericType
  * @tparam N
  * @tparam M
  */
trait TestGenTerm[N <: NumericType[M], M]
  extends GenTermTrait[N, M]
    with Matchers { this: FlatSpec =>

  "GenLeaf.WithoutVariable.isVariableGen" should
    "return true for genVariable" in  {
    GenLeaf.WithoutVariable.isVariableGen(genVariable) shouldEqual true
  }

  it should "return true for everything else" in {

    val otherTerms: Seq[Gen[Term]] = Seq(Gen.oneOf(Seq(IdentityFunction)))

    val numericTermLeafPossibilities: Seq[Gen[NumericTerm[N, M]]] = Seq(genRaw, genLiteral)
    val genLeafPossibilities: Seq[Gen[Term]] = numericTermLeafPossibilities ++ otherTerms

    val others: Seq[Gen[Any]] = Seq(genM, genRaw, genStr, genNumericTermBranch)

    (otherTerms ++ numericTermLeafPossibilities ++ genLeafPossibilities ++ others) foreach {
      thisGen => GenLeaf.WithoutVariable.isVariableGen(thisGen) shouldEqual false
    }
  }
}
