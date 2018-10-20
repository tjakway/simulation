package com.jakway.term.test

import com.jakway.term.TermOperations
import com.jakway.term.numeric.types.NumericType
import org.scalatest.{FlatSpec, Matchers}

abstract class TestFindVariables[N <: NumericType[M], M]
  (override val numericType: N)
  extends FlatSpec with Matchers with NumericTypeTest[N, M] {

  "findVariables" should "return Seq() for literals" in {
    //TermOperations.findVariables[N, M]()
  }
}
