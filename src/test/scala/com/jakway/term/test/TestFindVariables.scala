package com.jakway.term.test

import scala.reflect.runtime.universe._
import com.jakway.term.{TermOperations, Variable}
import com.jakway.term.numeric.types.NumericType
import org.scalatest.{FlatSpec, Matchers}

abstract class TestFindVariables[N <: NumericType[M], M]
  (override val numericType: N)
  extends FlatSpec with Matchers with NumericTypeTest[N, M] {

  val expr = new Expressions[N, M]()

  "findVariables" should "return Seq() for literals" in {
    TermOperations
      .findVariables[N, M](expr.addTwoLiterals) shouldEqual Seq()
  }

  it should "find variables in an unnested expression" in {
    TermOperations
      .findVariables[N, M](expr.addTwoVariables.term) shouldEqual
        expr.addTwoVariables.variables
  }
}
