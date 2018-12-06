package com.jakway.term.test

import scala.reflect.runtime.universe._
import com.jakway.term.TermOperations
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.test.framework.Expression
import org.scalatest.{FlatSpec, Matchers}

trait TestFindVariables[N <: NumericType[M], M]
  extends Matchers { this: FlatSpec =>

  private val expr = new TestExpressions[N, M]()

  def testExpression(e: Expression[N, M]): Unit = {
    TermOperations.findVariables[N, M](e.term) shouldEqual
      e.variables
  }

  "findVariables" should "return Seq() for literals" in {
    TermOperations
      .findVariables[N, M](expr.addTwoLiterals) shouldEqual Seq()
  }

  it should "find variables in an unnested expression" in {
    testExpression(expr.addTwoVariables)
  }

  it should "find variables in a nested expression 1 level deep" in {
    testExpression(expr.addThreeVariables)
  }

  it should "find variables in a deeply nested expression" in {
    testExpression(expr.deeplyNestedVariables)
  }
}
