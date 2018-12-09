package com.jakway.term.test.framework.gen

import com.jakway.term.numeric.types.NumericType
import com.jakway.term.solver.Solvable
import org.scalacheck.Gen

class GenSolvable[N <: NumericType[M], M]
  (val numericType: N) {
  val outerNumericType = numericType

  private val genTerm: GenTerm[N, M] = new GenTerm[N, M] {
    val numericType: N = outerNumericType
  }

  def genSolvable: Gen[Solvable] = {
    for {
      left <- genTerm.genNumericTerm()
      right <- genTerm.genNumericTerm()
    } yield Solvable(left, right)
  }
}

